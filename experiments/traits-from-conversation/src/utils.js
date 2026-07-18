import { doc, runTransaction, getDoc, setDoc, updateDoc, collection, addDoc } from 'firebase/firestore';
import { get } from 'svelte/store';
import { userStore, participantsCollectionName, metaCollectionName, db, globalVars, groupStore, groupsCollectionName, serverTime, taskVer, questionBank, nQuestions, questionPromptingConfig, questionStatusFilter, sessionId, studyId, prolificCompletionUrl } from './globals';
import { uniqueNamesGenerator, colors, adjectives } from 'unique-names-generator';

export const scrollWindowToTop = () => {
    // Scroll to the top of the page
    window.scrollTo(0, 0);
}

// Shuffle function using Fisher-Yates algorithm
export const shuffleArray = (array) => {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
};


export const initUser = async (prolificId) => {
  console.log("initUser -- prolificId", prolificId);
  console.log("initUser -- db", db);
  try {
      // We could have just tried to read the value of the $prolificId store here, but the $
      // syntax only works in .svelte files. There's a special get() function we have to
      // use instead, but because this is such simple case let's just make the prolificId like
      // we do in Login.svelte and avoid the overhead.
      const userDocRef = doc(db, participantsCollectionName, prolificId);

      // Check whether prolificId exists in partcipants doc first
      const userDocSnapshot = await getDoc(userDocRef);

      if (userDocSnapshot.exists()) {
          console.log(`User doc already exists for ${prolificId}`);
      } else {
          console.log(`Creating user doc for ${prolificId}...`);

          // Write user doc in participations collection
          await setDoc(userDocRef, {
              prolificId: prolificId,
              sessionId: sessionId,
              studyId: studyId,
              groupId: '',
              currentState: 'user-created',
              taskVer: taskVer,
              earlyExit: false
          });
      }
      console.log(`'New user ${prolificId} successfully created with document ID ${userDocRef.id}`)
  } catch (error) {
      console.log(`Error adding new user ${prolificId} doc to participants collection`);
      console.error(error);
  }
};


export const reqUserStateChange = async (newState) => {
  const { prolificId } = get(userStore);
  const { currentState } = get(groupStore);
  const groupState = currentState;
  const docRef = doc(db, participantsCollectionName, prolificId);

  // update user doc
  try {
      await runTransaction(db, async (transaction) => {
          // Get the latest data, rather than relying on the store
          const document = await transaction.get(docRef);
          if (!document.exists()) {
              throw "Document does not exist!";
          }
          // Freshest data
          const { currentState, groupId } = document.data();
          console.log(
              `Participant: ${prolificId} is requesting state change: ${currentState} -> ${newState}`
          );

          const data = {};
          // To account for incognito users who may have closed the window
          // and return, we need to check if they're in a group
          // If they are, we don't want to update their userDoc state
          // as they need to instead be redirected to the group state
          if (groupId == "") {
              console.log("User is not in a group, so updating user doc state");
              data["currentState"] = newState;
          } else if (groupState == "start-chat" || groupState == "chat-complete") {
            console.log("User is in debrief state, so updating user doc state");
            data["currentState"] = newState;
          }
          data[`${currentState}_end`] = serverTime;
          data[`${newState}_start`] = serverTime;
          console.log('Initiating state change...');
          await transaction.update(docRef, data);
          console.log('Ran transaction...');
      });
  } catch (error) {
    console.error(`Error updating state to ${newState} for user: ${prolificId}`, error);
  }
};

// Update the experiment state and write to firebase
// Checks to see if all participants are ready to transition from state A -> B each
// time the function runs. So only the *last* user to call this function actually
// initiates the state change
export const reqStateChange = async (newState) => {
  const { prolificId } = get(userStore);
  const { groupId } = get(groupStore);
  const docRef = doc(db, groupsCollectionName, groupId);

  // update group doc
  try {
    await runTransaction(db, async (transaction) => {

      // Get the latest data, rather than relying on the store
      const document = await transaction.get(docRef);
      if (!document.exists()) {
        throw "Document does not exist!";
      }
      // Freshest data
      const { counter, currentState } = document.data();
      console.log(
        `Participant: ${prolificId} is requesting state change: ${currentState} -> ${newState}`
      );
      const data = { counter: [...counter, prolificId] };

      // Add the user to the counter if they're not already in it
      if (!counter.includes(prolificId)) {
        await transaction.update(docRef, data);
      } else {
        console.log("Ignoring duplicate request");
      }
    });
  } catch (error) {
    console.error(`Error updating state to ${newState} for group: ${groupId}`, error);
  }
  // Use helper function to run a second transaction that checks the counter length and
  // actually performs the state change if appropriate
  await verifyStateChange(newState);
};

// Helper function called by reqStateChange that runs a follow-up transaction after the
// reqStateChange transaction so we rely on freshes counter value in the db rather than
// the value in any user's store which may be out-of-sync
// Also has the benefit that if all users have requested a state change, but it failed
// for some reason, then any user can re-make that request without overwriting their
// data and it will run successfully
const verifyStateChange = async (newState) => {
  const { groupId } = get(groupStore);
  const docRef = doc(db, groupsCollectionName, groupId);
  try {
    await runTransaction(db, async (transaction) => {
      // Get the latest data, rather than relying on the store
      const document = await transaction.get(docRef);
      if (!document.exists()) {
        throw "Document does not exist!";
      }
      // Get latest counter
      const { counter } = document.data();
      if (counter.length === globalVars.groupSize) {
        console.log('Last request... change');
        const obj = {};
        // re-initialize counter as empty array
        obj["counter"] = [];
        obj["currentState"] = newState;
        await transaction.update(docRef, obj);
      } else {
        console.log(`Still waiting for ${globalVars.groupSize - counter.length} requests...`);
      }
    });
  } catch (error) {
    console.error(`Error verifying state change`, error);
  }
};



// Save slider responses
export const saveSliderResponses = async (prolificId, responseKey, responses) => {
    console.log("saveSliderResponses -- prolificId", prolificId);
    console.log("saveSliderResponses -- responses", responses);
    const userDocRef = doc(db, participantsCollectionName, prolificId);
    try {
        await updateDoc(userDocRef, {
            [responseKey]: responses,
        });
        console.log(`Successfully saved survey responses for user ${prolificId}`);
    } catch (error) {
        console.error(`Error saving survey responses for user ${prolificId}`, error);
    }
}


// Save survey response
export const saveSurveyResponses = async (prolificId, responses) => {
  const userDocRef = doc(db, participantsCollectionName, prolificId);

  try {
    await runTransaction(db, async (transaction) => {
      const changes = {};
      responses.forEach((item) => {
        console.log(`Question ${item.index}: ${item.question}`);
        console.log(`Response: ${item.response}`);
        console.log('---');

        changes[`demographics.${item.question}`] = item.response;
      });

      transaction.update(userDocRef, changes);
    });

    console.log(`Successfully saved survey responses for user ${prolificId}`);
  } catch (error) {
    console.error(`Error saving survey responses for user ${prolificId}`, error);
  }
};


// Save responses to feedback and technical difficulties questions
export const saveFeedback = async (prolificId, sliderGetToKnowPartner, sliderPartnerPredictionAccuracy, technical, feedback) => {
  const userDocRef = doc(db, participantsCollectionName, prolificId);

  try {
    await runTransaction(db, async (transaction) => {
      const changes = {};

      console.log(`How well do you think you got to know your conversation partner? ${sliderGetToKnowPartner.response}`);
      console.log(`How accurate do you think your predictions about your conversation partner were in the previous task? ${sliderPartnerPredictionAccuracy.response}`);
      console.log(`Technical difficulties: ${technical.response}`);
      console.log(`Feedback: ${feedback.response}`);
      console.log('---');

      changes[`sliderGetToKnowPartner`] = sliderGetToKnowPartner.response;
      changes[`sliderPartnerPredictionAccuracy`] = sliderPartnerPredictionAccuracy.response;
      changes[`technical`] = technical.response;
      changes[`feedback`] = feedback.response;

      transaction.update(userDocRef, changes);
    });

    console.log(`Successfully saved feedback for user ${prolificId}`);
  } catch (error) {
    console.error(`Error saving feedback data for user ${prolificId}`, error);
  }
};


// For when someone times out in the lobby
export const removeUserFromLobby = async (prolificId) => {
  const metaDocRef = doc(db, metaCollectionName, 'data');

  // get latest counter from meta doc
  try {
    await runTransaction(db, async (transaction) => {

      // Get the latest data, rather than relying on the store
      const document = await transaction.get(metaDocRef);
      if (!document.exists()) {
        console.log("Meta doc doesn't exist, nothing to remove from");
        return;
      }
      // Freshest data
      let { counter } = document.data();

      // Remove the prolificId from the counter array
      counter = counter.filter((id) => id !== prolificId);
      const data = { counter: [...counter] };

      // Update meta doc
      await transaction.update(metaDocRef, data);

    });
  } catch (error) {
    console.error(`Error removing prolificId from metaDoc counter: ${prolificId}`, error);
  }

}


// add prolificId to meta counter
export const reqMetaDocChange = async (prolificId) => {
  const metaDocRef = doc(db, metaCollectionName, 'data');

  // update meta doc
  try {
    await runTransaction(db, async (transaction) => {

      // Get the latest data, rather than relying on the store
      const document = await transaction.get(metaDocRef);

      // Auto-create meta doc if it doesn't exist
      if (!document.exists()) {
        transaction.set(metaDocRef, { counter: [prolificId] });
        console.log("Created meta doc with first user in counter");
        return;
      }

      // Freshest data
      const { counter } = document.data();
      const data = { counter: [...counter, prolificId] };

      // Add the user to the counter if they're not already in it
      if (!counter.includes(prolificId)) {
        await transaction.update(metaDocRef, data);
      } else {
        console.log("Ignoring duplicate request");
      }
    });
  } catch (error) {
    console.error(`Error updating meta doc for prolificId: ${prolificId}`, error);
  }
  // Use helper function to run a second transaction that checks the counter length and
  // actually performs the state change if appropriate
  await checkIfResetCounter(prolificId);
};


// helper funtion to run check for meta doc change
// every time user joins, check if counter is full
const checkIfResetCounter = async () => {
  // Get latest counter
  let counter = [];

  const metaDocRef = doc(db, metaCollectionName, 'data');
  try {
    await runTransaction(db, async (transaction) => {
      // Get the latest data, rather than relying on the store
      const document = await transaction.get(metaDocRef);
      if (!document.exists()) {
        console.log("Meta doc doesn't exist, skipping counter check");
        return;
      }
      // Get latest counter
      counter = document.data().counter;

      // group size met
      if (counter.length === globalVars.groupSize) {
        // if you're the last user to join the group,
        // you initialize the group doc
        console.log('Resetting counter...');
        // reset counter
        const obj = {};
        obj["counter"] = [];
        await transaction.update(metaDocRef, obj);
    } else {
        console.log("No need to reset. Still waiting for users to join...");
      }
    });
  } catch (error) {
    console.error(`Error verifying meta doc change`, error);
  }

  if (counter.length === globalVars.groupSize) {
    console.log('Initializing group...');
    // initialize group
    await initGroup(counter);
  } else if (counter.length > globalVars.groupSize) {
    console.log(`Counter length ${counter.length} exceeds group size`);
    console.log("counter", counter)
  } else {
    console.log(`Still waiting for ${globalVars.groupSize - counter.length} requests...`);
  }

};


// To initialize/update the group document in the database
export const initGroup = async (counter) => {
  try {
    let groupId;

    // Generate a unique groupId
    // Loop until we find one that does NOT exist in Firestore
    while (!groupStore["groupId"]) {
      const roomGenConfig = {
        dictionaries: [adjectives, colors],
        style: 'lowerCase',
        separator: '-'
      };
      groupId = uniqueNamesGenerator(roomGenConfig);

      // Check if the group document exists
      const groupDocRef = doc(db, groupsCollectionName, groupId);
      const groupDocSnapshot = await getDoc(groupDocRef);

      if (!groupDocSnapshot.exists()) {
        console.log(`Group doc does not exist for ${groupId} — using this ID.`);
        break; // ID is unique → proceed
      } else {
        console.log(`Group doc already exists for ${groupId}, generating a new one...`);
      }
    }

    console.log("initGroup -- UNIQUE groupId:", groupId);

    // Create new group document
    const groupDocRef = doc(db, groupsCollectionName, groupId);
    console.log(`Group ${groupId} document does not exist! Creating now...`);

    // Assign chat condition (deep vs shallow) - 50/50 random
    const chatCondition = Math.random() < 0.5 ? 'deep' : 'shallow';
    console.log("initGroup -- chatCondition:", chatCondition);

    // Sample questions based on condition
    const categoryFilter = chatCondition === 'deep' ? 'personal' : 'small talk';
    const eligibleQuestions = questionBank.filter(q => q.category === categoryFilter && q.status === questionStatusFilter);
    const sampledQuestions = shuffleArray([...eligibleQuestions]).slice(0, nQuestions);

    // Build questions map with index for display order
    // {questionId: {text, index, chosenByProlificId, chosenAtRound}}
    const questionsMap = {};
    sampledQuestions.forEach((q, index) => {
      questionsMap[q.question_id] = {
        text: q.question,
        index: index,                  // 0-4 for display order
        chosenByProlificId: null,          // null, or id who selected it
        chosenAtRound: null,           // null, or round number when chosen (1-5)
      };
    });
    console.log("initGroup -- questionsMap:", questionsMap);

    // Set initial roles based on lobby queue order
    // counter[0] = first to join lobby = first asker
    const currentAskerId = counter[0];
    const currentAnswererId = counter[1];

    await setDoc(groupDocRef, {
      groupId: groupId,
      numUsers: counter.length,
      prolificIds: counter,
      avatars: globalVars.avatars,
      avatarNames: globalVars.avatarNames,
      counter: [],
      currentState: 'start-chat',
      startTime: serverTime,
      typing: {
        [counter[0]]: false,
        [counter[1]]: false,
      },
      taskVer: taskVer,
      ...(taskVer === 'questionPrompting' && {
        // Question prompting config (static, for data analysis)
        questionPromptingConfig: questionPromptingConfig,
        // Question prompting state (nested object, updated during session)
        questionPrompting: {
          // Condition & Questions
          condition: chatCondition,
          questions: questionsMap,
          // Current roles (swap each round)
          currentAskerId: currentAskerId,
          currentAnswererId: currentAnswererId,
          // Round tracking
          // We can start at 1 bc this is a map keyed by round number, not an array index
          currentRound: 1,               // 1-5 = active rounds
          currentQuestionId: null,
          // History of roles per round (populated when each round starts)
          // Should look like this:
          // { [roundNumber]: { askerId, answererId, questionId } }
          rounds: {},
          // Phase tracking
          phase: 'selecting',            // 'selecting' | 'discussing' | 'waiting_for_next' | 'complete'
          // Timers (store start times, calculate remaining client-side)
          globalTimerStart: serverTime,
          phaseTimerStart: serverTime,
          // Next question consent tracking
          counter: [],                   // array of uids who clicked "Next Question"
      }})
    });

    console.log(`New group ${groupId} created with document ID: ${groupDocRef.id}`);

    // Assign groupId to each user document
    for (const id of counter) {
      const userDocRef = doc(db, participantsCollectionName, id);
      await updateDoc(userDocRef, {
        groupId: groupId,
        currentState: 'start-chat',
      });
      console.log(`Assigned groupId ${groupId} to id ${id}`);
    }

  } catch (error) {
    console.error("initGroup error:", error);
  }
};


// ============================================
// Question Prompting-Specific Utility Functions
// These are only actively used when taskVer === 'questionPrompting'
// ============================================


/**
 * Submit the selected question for the current round.
 * - Records round roles in rounds history
 * - Marks the question as chosen in the questions map
 * - Sets phase to 'discussing'
 * - Updates phaseTimerStart
 * - Sets currentQuestionId
 * - Posts the question as the first chat message from the asker
 */
export const submitQuestion = async (groupId, questionId, prolificId) => {
  const groupDocRef = doc(db, groupsCollectionName, groupId);

  try {
    await runTransaction(db, async (transaction) => {
      const document = await transaction.get(groupDocRef);
      if (!document.exists()) {
        throw "Group document does not exist!";
      }

      const { questionPrompting } = document.data();
      const { questions, currentRound, phase, currentAskerId, currentAnswererId, rounds } = questionPrompting;

      // Only allow submission during 'selecting' phase
      if (phase !== 'selecting') {
        console.log("Cannot submit question: not in selecting phase");
        return;
      }

      // Record this round's roles in history
      const updatedRounds = { ...rounds };
      updatedRounds[currentRound] = {
        askerId: currentAskerId,
        answererId: currentAnswererId,
        questionId: questionId,
      };

      // Update the question as chosen
      const updatedQuestions = { ...questions };
      if (updatedQuestions[questionId]) {
        updatedQuestions[questionId] = {
          ...updatedQuestions[questionId],
          chosenByProlificId: prolificId,
          chosenAtRound: currentRound,
        };
      }

      // Update group doc (using dot notation for nested fields)
      transaction.update(groupDocRef, {
        'questionPrompting.rounds': updatedRounds,
        'questionPrompting.questions': updatedQuestions,
        'questionPrompting.currentQuestionId': questionId,
        'questionPrompting.phase': 'discussing',
        'questionPrompting.phaseTimerStart': serverTime,
      });

      console.log(`Question ${questionId} submitted for round ${currentRound}`);
    });

    // Post the question as the first chat message from the asker
    const groupDoc = await getDoc(groupDocRef);
    const questionText = groupDoc.data().questionPrompting.questions[questionId]?.text;
    if (questionText) {
      const messageObj = {
        author: prolificId,
        message_string: questionText,
        group_id: groupId,
        isQuestionPrompt: true,  // flag to style differently in chat
      };
      await addMessage(groupId, messageObj);
    }

  } catch (error) {
    console.error("submitQuestion error:", error);
  }
};


/**
 * Request to move to the next question.
 * - Adds prolificId to questionPrompting.counter
 * - If both users have clicked, triggers swapRolesAndAdvance
 */
export const requestNextQuestion = async (groupId, prolificId) => {
  const groupDocRef = doc(db, groupsCollectionName, groupId);

  try {
    let shouldAdvance = false;

    await runTransaction(db, async (transaction) => {
      const document = await transaction.get(groupDocRef);
      if (!document.exists()) {
        throw "Group document does not exist!";
      }

      const { questionPrompting } = document.data();
      const { counter, phase } = questionPrompting;

      // Only allow during 'discussing' phase or 'one_ready_for_next' phase
      if (phase !== 'discussing' && phase !== 'one_ready_for_next') {
        console.log("Cannot request next question: not in discussing or one_ready_for_next phase");
        return;
      }

      // Add prolificId to counter if not already there
      if (!counter.includes(prolificId)) {
        const updatedCounter = [...counter, prolificId];

        if (updatedCounter.length >= globalVars.groupSize) {
          // Both users ready - set phase to waiting, will advance after transaction
          transaction.update(groupDocRef, {
            'questionPrompting.counter': updatedCounter,
            'questionPrompting.phase': 'waiting_for_next',
          });
          shouldAdvance = true;
        } else {
          // First user clicked - only update counter, move phase to 'one_ready_for_next' (allows partner to see that this user is ready for the next question)
          transaction.update(groupDocRef, {
            'questionPrompting.counter': updatedCounter,
            'questionPrompting.phase': 'one_ready_for_next',
          });
        }

        console.log(`User ${prolificId} requested next question. Counter: ${updatedCounter.length}/${globalVars.groupSize}`);
      }
    });

    // If both users are ready, advance to next round
    if (shouldAdvance) {
      await swapRolesAndAdvance(groupId);
    }

  } catch (error) {
    console.error("requestNextQuestion error:", error);
  }
};


/**
 * Swap roles and advance to next round.
 * - Increments currentRound
 * - Swaps currentAskerId and currentAnswererId
 * - Clears counter
 * - Sets phase back to 'selecting'
 * - Resets phaseTimerStart
 * - Clears currentQuestionId
 */
export const swapRolesAndAdvance = async (groupId) => {
  const groupDocRef = doc(db, groupsCollectionName, groupId);

  try {
    await runTransaction(db, async (transaction) => {
      const document = await transaction.get(groupDocRef);
      if (!document.exists()) {
        throw "Group document does not exist!";
      }

      const { questionPrompting } = document.data();
      const { currentAskerId, currentAnswererId, currentRound, questions, condition } = questionPrompting;

      // Check how many unchosen questions remain
      const unchosenCount = Object.values(questions).filter(q => q.chosenByProlificId === null).length;

      // Refresh bank when only 1 unchosen question remains
      if (unchosenCount === 1) {
        console.log(`Only ${unchosenCount} unchosen question(s) remaining - refreshing question bank`);

        // Get all previously chosen question IDs
        const chosenQuestionIds = Object.entries(questions)
          .filter(([_, q]) => q.chosenByProlificId !== null)
          .map(([id, _]) => id);

        console.log("Previously chosen questions:", chosenQuestionIds);

        // Sample new questions, excluding already chosen ones
        const categoryFilter = condition === 'deep' ? 'personal' : 'small talk';
        const eligibleQuestions = questionBank.filter(q =>
          q.category === categoryFilter &&
          q.status === questionStatusFilter &&
          !chosenQuestionIds.includes(q.question_id)
        );

        // Check if we have ANY questions left
        if (eligibleQuestions.length === 0) {
          console.log("All questions have been discussed - ending chat");
          transaction.update(groupDocRef, {
            'questionPrompting.phase': 'complete',
            'questionPrompting.counter': [],
          });
          return;
        }

        // Sample new questions (take all available if less than nQuestions)
        const numQuestionsToSample = Math.min(eligibleQuestions.length, nQuestions);
        const newSampledQuestions = shuffleArray([...eligibleQuestions]).slice(0, numQuestionsToSample);

        // Build new questions map, preserving ALL existing questions (both chosen and unchosen)
        const newQuestionsMap = {};

        // Preserve all existing questions
        Object.entries(questions).forEach(([id, q]) => {
          newQuestionsMap[id] = q;
        });

        // Count how many available (unchosen) questions we still have
        const existingAvailableCount = Object.values(newQuestionsMap).filter(
          q => q.chosenByProlificId === null
        ).length;

        // Always add nQuestions new questions (or all available if less than nQuestions)
        // This means remaining unchosen questions are preserved AND we add 5 new ones
        const actualNewQuestions = newSampledQuestions;

        // Add new questions
        let newIndex = Object.keys(newQuestionsMap).length;
        actualNewQuestions.forEach((q) => {
          newQuestionsMap[q.question_id] = {
            text: q.question,
            index: newIndex++,
            chosenByProlificId: null,
            chosenAtRound: null,
          };
        });

        console.log("Refreshed question bank with", actualNewQuestions.length, "new questions +", existingAvailableCount, "existing unchosen =", actualNewQuestions.length + existingAvailableCount, "total available");

        // Continue with role swap and round advancement
        const newRound = currentRound + 1;

        transaction.update(groupDocRef, {
          'questionPrompting.questions': newQuestionsMap,
          'questionPrompting.currentRound': newRound,
          'questionPrompting.currentAskerId': currentAnswererId,      // swap
          'questionPrompting.currentAnswererId': currentAskerId,      // swap
          'questionPrompting.currentQuestionId': null,
          'questionPrompting.phase': 'selecting',
          'questionPrompting.phaseTimerStart': serverTime,
          'questionPrompting.counter': [],
        });

        console.log(`Refreshed questions and advanced to round ${newRound}. New asker: ${currentAnswererId}`);
        return;
      }

      const newRound = currentRound + 1;

      // Swap roles and advance to next round
      transaction.update(groupDocRef, {
        'questionPrompting.currentRound': newRound,
        'questionPrompting.currentAskerId': currentAnswererId,      // swap
        'questionPrompting.currentAnswererId': currentAskerId,      // swap
        'questionPrompting.currentQuestionId': null,
        'questionPrompting.phase': 'selecting',
        'questionPrompting.phaseTimerStart': serverTime,
        'questionPrompting.counter': [],
      });

      console.log(`Advanced to round ${newRound}. New asker: ${currentAnswererId}`);
    });

  } catch (error) {
    console.error("swapRolesAndAdvance error:", error);
  }
};


// update firebase when participant is typing
export const notifyPartnerTyping = (groupId, prolificId, isTyping) => {
  const groupDocRef = doc(db, groupsCollectionName, groupId);
  updateDoc(groupDocRef, {
    [`typing.${prolificId}`]: isTyping,
  });
}


// Function to add a new message to the group doc
export const addMessage = async (groupDocName, messageObj) => {
  // Have to do this here bc serverTimestamp()
  // serverTimestamp() is not currently supported inside arrays
  // add absolute timestamp to messageObj
  messageObj['absolute_timestamp'] = serverTime;

  // Access the group doc
  const groupDocRef = doc(db, groupsCollectionName, groupDocName);
  const groupDocSnapshot = await getDoc(groupDocRef);
  const messagesCollectionRef = collection(groupDocRef, "messages");

  // Check if the group doc already exists
  if (groupDocSnapshot.exists()) {
    const groupData = groupDocSnapshot.data();

    // Add Question Under Discussion (qud) and related fields
    // These fields are always present regardless of taskVer
    messageObj['taskVer'] = groupData.taskVer || null;
    messageObj['qud'] = groupData.questionPrompting?.currentQuestionId || null;
    messageObj['currentRound'] = groupData.questionPrompting?.currentRound || null;
    messageObj['phase'] = groupData.questionPrompting?.phase || null;

    console.log(`Group doc already exists for ${groupDocName}`);

    // Update messages for group doc
    try {
      await updateDoc(groupDocRef, {
        newMessage: true
      });
      // Add the message to the "messages" collection as a new document
      await addDoc(messagesCollectionRef, messageObj);
      console.log(`New message successfully added to group ${groupDocName}`);
    } catch (error) {
      console.log(error)
    }
  }
};

/**
 * Handle early exit from chat due to inactive partner
 * - Marks both users as early exit
 * - Moves both users to partner-survey-complete state
 * - Updates group state to chat-complete
 */
export const handlePartnerInactiveExit = async (prolificId, partnerProlificId, groupId) => {
  try {
    // Update current user document
    const userDocRef = doc(db, participantsCollectionName, prolificId);
    await updateDoc(userDocRef, {
      earlyExit: true,
      currentState: 'partner-survey-complete'
    });

    // Update partner document (don't mark partner as earlyExit - they may have been active)
    const partnerDocRef = doc(db, participantsCollectionName, partnerProlificId);
    await updateDoc(partnerDocRef, {
      currentState: 'partner-survey-complete'
    });

    // Update group state
    const groupDocRef = doc(db, groupsCollectionName, groupId);
    await updateDoc(groupDocRef, {
      currentState: 'chat-complete'
    });

    console.log(`Early exit handled for users ${prolificId} and ${partnerProlificId} in group ${groupId}`);
    return true;
  } catch (error) {
    console.error("Error handling partner inactive exit:", error);
    return false;
  }
};


// function to redirect participants to prolific upon completion
export const redirectToProlific = async () => {
  const url = prolificCompletionUrl

  if (!url) {
    console.error("Prolific redirect URL missing!");
    return;
  }

  console.log("Redirecting to:", url);
  location.replace(url);
};