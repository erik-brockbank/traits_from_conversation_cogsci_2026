<script>
  import {
    // firebaseConfig,
    participantsCollectionName,
    groupsCollectionName,
    metaCollectionName,
    db,
    auth,
    prolificId,
    userStore,
    groupStore,
    groupMessagesStore,
  } from "./globals";
  import {
    initUser,
    reqUserStateChange,
    reqStateChange,
    redirectToProlific,
  } from "./utils";

  import { onAuthStateChanged, signInAnonymously } from "firebase/auth";
  import {
    collection,
    doc,
    onSnapshot,
    query,
    orderBy,
  } from "firebase/firestore";

  import Chat from "./pages/Chat.svelte";
  import Consent from "./pages/Consent.svelte";
  import Demographics from "./pages/Demographics.svelte";
  import EndExperiment from "./pages/EndExperiment.svelte";
  import Feedback from "./pages/Feedback.svelte";
  import Fullscreen from "./pages/Fullscreen.svelte";
  import Instructions from "./pages/Instructions.svelte";
  import Landing from "./pages/Landing.svelte";
  import Lobby from "./pages/Lobby.svelte";
  import PersonalitySurvey from "./pages/PersonalitySurvey.svelte";
  import MatchFail from "./pages/MatchFail.svelte";
  import Loading from "./components/Loading.svelte";
  import ReturnStudy from "./pages/ReturnStudy.svelte";



  // Sign in anonymously when the app loads
  signInAnonymously(auth)
    .then(() => {
      console.log("Signed in anonymously");
    })
    .catch((error) => {
      console.error("Anonymous auth error:", error);
    });

  onAuthStateChanged(auth, (user) => {
    if (user) {
      console.log("prolificId", prolificId);
      // User is signed in, user object is truthy
      try {
        // Subscribe to their user doc
        onSnapshot(doc(db, participantsCollectionName, prolificId), (userDoc) => {
          if (userDoc.exists()) {
            console.log("User doc exists with data: ", userDoc.data());
            userStore.set(userDoc.data());
            // Check if groupId exists, if it does, create group subscription
            // that way group doc will run if user doc changes,
            // always looking to check if they've been assigned to a group
            const groupId = userDoc.data()?.groupId;
            console.log("groupId", groupId);
            if (groupId != "") {
              // Also subscribe to their group doc
              onSnapshot(doc(db, groupsCollectionName, groupId), (groupDoc) => {
                groupStore.set(groupDoc.data());
              });
              console.log(`User ${prolificId} subbed to group ${groupId} data`);
              let groupDocRef = doc(db, groupsCollectionName, groupId);
              let messagesCollectionRef = collection(groupDocRef, "messages");
              // Subscribe to messages collection
              let messagesQuery = query(
                messagesCollectionRef,
                orderBy("absolute_timestamp", "asc")
              );
              onSnapshot(messagesQuery, (querySnapshot) => {
                const groupMessages = querySnapshot.docs.map(
                  (groupMessagesDoc) => groupMessagesDoc.data()
                );
                groupMessagesStore.set(groupMessages);
              });
            }
          } else {
            console.log("User doc does not exist for prolificId: ", prolificId);
          }
        });
      } catch (error) {
        console.error("Error subscribing to user doc: ", error);
      }
    } else {
      // User is signed out
      console.log("Auth state changed: No user is signed in.");
    }
  });

  const createUser = async () => {
    console.log("App -- createUser -- prolificId", prolificId);
    await initUser(prolificId);
    updateUserState("user-created");
  };

  const updateUserState = async (newState) => {
    console.log(
      "App -- updateUserState -- currentState",
      $userStore["currentState"]
    );
    console.log("App -- updateUserState -- newState", newState);
    await reqUserStateChange(newState);
  };

  const updateState = async (newState) => {
    console.log(
      "App -- updateState -- currentState",
      $groupStore["currentState"]
    );
    console.log("App -- updateState -- newState", newState);
    await reqStateChange(newState);

    if (newState == "chat-complete") {
      updateUserState("chat-complete");
    }
  };
</script>

<main>
  {#if !$userStore["currentState"]}
    <Landing
      on:finished={() => {
        // NB: updateUserState is called in createUser to avoid race condition
        createUser();
      }}
    />
  {:else if $userStore["currentState"] === "user-created"}
    <!-- TODO: give user option to not consent to participate -->
    <Consent
      on:finished={() => {
        updateUserState("consent-confirmed");
      }}
      on:reject={() => {
        updateUserState("return-study");
      }}
    />
  {:else if $userStore["currentState"] === "return-study"}
      <ReturnStudy></ReturnStudy>
  {:else if $userStore["currentState"] === "consent-confirmed"}
    <Fullscreen
      on:finished={() => {
        updateUserState("fullscreen-confirmed");
      }}
    />
  {:else if $userStore["currentState"] === "fullscreen-confirmed"}
    <Instructions
      instructionSet={"instructions-1"}
      on:finished={() => {
        updateUserState("instructions-1-complete");
      }}
    />
  {:else if $userStore["currentState"] === "instructions-1-complete"}
    <PersonalitySurvey
      surveyData={{ surveyType: "self" }}
      on:finished={() => {
        updateUserState("self-survey-complete");
      }}
    />
  {:else if $userStore["currentState"] === "self-survey-complete"}
    <Instructions
      instructionSet={"instructions-2"}
      on:finished={() => {
        updateUserState("instructions-2-complete");
      }}
    />
  {:else if $userStore["currentState"] === "instructions-2-complete"}
    <Lobby on:match-failed={() => updateUserState("match-fail")} />
  {:else if $userStore["currentState"] === "match-fail"}
    <MatchFail
      on:redirect-to-prolific={() => redirectToProlific()}
      on:redirect-to-lobby={() => updateUserState("instructions-2-complete")}
    />
    <!-- Synchronized states: use groupStore for chat -->
  {:else if $groupStore["currentState"] === "start-chat"}
    <Chat
      on:finished={() => {
        updateState("chat-complete");
        updateUserState("chat-complete");
      }}
    />
    <!-- Post-chat: back to userStore for individual progress -->
  {:else if $userStore["currentState"] === "chat-complete"}
    <Instructions
      instructionSet={"instructions-3"}
      on:finished={() => {
        updateUserState("instructions-3-complete");
      }}
    />
  {:else if $userStore["currentState"] === "instructions-3-complete"}
    <PersonalitySurvey
      surveyData={{ surveyType: "partner" }}
      on:finished={() => {
        updateUserState("partner-survey-complete");
      }}
    />
  {:else if $userStore["currentState"] === "partner-survey-complete"}
    <Instructions
      instructionSet={"instructions-4"}
      on:finished={() => {
        updateUserState("instructions-4-complete");
      }}
    />
  {:else if $userStore["currentState"] === "instructions-4-complete"}
    <Demographics
      on:finished={() => {
        updateUserState("demographics-complete");
      }}
    />
  {:else if $userStore["currentState"] === "demographics-complete"}
    <Feedback
      on:finished={() => {
        updateUserState("feedback-complete");
      }}
    />
  {:else if $userStore["currentState"] === "feedback-complete"}
    <EndExperiment
      on:finished={() => {
        updateUserState("experiment-complete");
        console.log("Finished experiment");
        redirectToProlific();
      }}
    />
  {:else}
    <Loading />
  {/if}
</main>
