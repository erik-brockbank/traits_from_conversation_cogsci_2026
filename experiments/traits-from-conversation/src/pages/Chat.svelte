<script>
  import {
    globalVars,
    groupStore,
    prolificId,
    userStore,
    groupMessagesStore,
    nQuestions,
    serverTime,
    taskVer,
    DEBUG,
  } from "../globals";
  import {
    notifyPartnerTyping,
    removeUserFromLobby,
    addMessage,
    handlePartnerInactiveExit,
  } from "../utils";
  import QuestionBank from "../components/QuestionBank.svelte";
  import { createEventDispatcher, onMount, onDestroy } from "svelte";
  import { Progressbar } from "flowbite-svelte";
  import { sineOut } from "svelte/easing";

  // TODO: Update to Svelte 5
  const dispatch = createEventDispatcher();
  let chatComplete = $state(false); // state variable for when chat is complete; disables Until next time! button while DB transaction is in process

  let chatCondition = $groupStore.questionPrompting?.condition;
  console.log("chat condition: ", chatCondition);

  let chatDuration = globalVars.chatDuration;

  // Questions are now stored in groupStore from initGroup
  // Convert questions map to array for display
  // Sort: available (unchosen) questions first, then discussed questions
  let questions = $derived(
    Object.entries($groupStore.questionPrompting?.questions || {})
      .map(([id, q]) => ({
        question_id: id,
        question: q.text,
        index: q.index,
        chosenByProlificId: q.chosenByProlificId,
        chosenAtRound: q.chosenAtRound,
      }))
      .sort((a, b) => {
        // Available questions (chosenByProlificId === null) should come first
        const aIsAvailable = a.chosenByProlificId === null;
        const bIsAvailable = b.chosenByProlificId === null;
        if (aIsAvailable && !bIsAvailable) return -1;
        if (!aIsAvailable && bIsAvailable) return 1;
        // Within each group, sort by index
        return a.index - b.index;
      })
  );

  let isAsker = $derived(
    prolificId === $groupStore.questionPrompting?.currentAskerId
  );
  let currentPhase = $derived($groupStore.questionPrompting?.phase);

  // log prolificId to console
  console.log("Chat.svelte -- prolificId --", prolificId);

  // get partner prolificId
  let partnerProlificId =
    $groupStore["prolificIds"].indexOf($userStore["prolificId"]) == 0
      ? $groupStore["prolificIds"][1]
      : $groupStore["prolificIds"][0];

  // upon new message, autoscroll to bottom of chat window
  const updateScroll = () => {
    const chatWindow = document.getElementById("chatWindow");
    setTimeout(() => {
      chatWindow.scrollTop = chatWindow.scrollHeight;
    }, 0);
  };

  // upon message submit, update chat log in window
  const handleSubmit = async () => {
    // remove preceeding and trailing whitespaces
    const trimmedMessage = message.message_string.trim();

    if (trimmedMessage == "") {
      return;
    }

    let messageObj = {
      author: prolificId,
      message_string: `${message.message_string}`,
      group_id: $groupStore["groupId"],
    };
    console.log("messageObj", messageObj);

    // Add message to the group document
    await addMessage($groupStore["groupId"], messageObj);

    // Reset typing notification once user has sent message
    notifyPartnerTyping($groupStore["groupId"], prolificId, false);

    updateScroll();
    message.message_string = "";
  };

  // Auto-scroll when new messages arrive
  // Track message count to trigger scroll on new messages
  $effect(() => {
    const messageCount = $groupMessagesStore?.length || 0;
    if (messageCount > 0) {
      updateScroll();
    }
  });

  // initialize chat timer - synchronized via Firestore startTime
  let now = $state(Date.now());
  let interval;

  // Get the chat start time from Firestore (stored as a Firestore Timestamp)
  // The startTime is set when the group is created in initGroup()
  let chatStartTime = $derived(
    $groupStore["startTime"]?.toMillis?.() || Date.now()
  );

  // Calculate remaining seconds based on the synchronized start time
  let remainingSeconds = $derived(
    Math.max(0, chatDuration * 60 - Math.floor((now - chatStartTime) / 1000))
  );
  let timerMinutes = $derived(Math.floor(remainingSeconds / 60));
  let timerSeconds = $derived(remainingSeconds % 60);

  // Calculate seconds passed
  let secondsPassed = $derived(chatDuration * 60 - remainingSeconds);

  // Format message timestamp as MM:SS since chat started (for debug mode)
  const formatRelativeTimestamp = (absoluteTimestamp) => {
    if (!absoluteTimestamp?.toMillis) return "";
    const messageTime = absoluteTimestamp.toMillis();
    const elapsedMs = messageTime - chatStartTime;
    if (elapsedMs < 0) return "00:00";
    const totalSeconds = Math.floor(elapsedMs / 1000);
    const minutes = Math.floor(totalSeconds / 60);
    const seconds = totalSeconds % 60;
    return `${String(minutes).padStart(2, "0")}:${String(seconds).padStart(2, "0")}`;
  };

  // Calculate percentage progress to put in the progress bar
  let barProgress = $derived((secondsPassed / (chatDuration * 60)) * 100);

  // How many seconds before chat ends to send warning message
  let secondsAfterWarning = globalVars.secondsAfterWarning;
  console.log("secondsAfterWarning", secondsAfterWarning);

  // Helper function to check inactivity
  const checkInactivity = () => {
    if (!hasRespondedToModal) {
      // For questionPrompting mode, only check during discussion phases
      if (taskVer === "questionPrompting") {
        // Only check inactivity if we're in a discussion phase
        if (currentPhase !== "discussing") {
          return; // Don't check during selection/waiting phases
        }

        // Get the phase timer start to know when discussion began
        const phaseStartTime =
          $groupStore.questionPrompting?.phaseTimerStart?.toMillis?.();
        if (!phaseStartTime) return;

        // Check if partner has sent messages during this discussion phase
        const partnerMessagesThisPhase = ($groupMessagesStore || []).filter(
          (m) =>
            m.author !== prolificId &&
            m.author !== "Server" &&
            m.absolute_timestamp?.toMillis?.() >= phaseStartTime
        );

        if (partnerMessagesThisPhase.length === 0) {
          // No messages from partner in this discussion phase
          const timeSincePhaseStart = Date.now() - phaseStartTime;
          if (
            timeSincePhaseStart > INACTIVITY_THRESHOLD &&
            !showInactivityModal
          ) {
            showInactivityModal = true;
          }
        }
      } else {
        // Baseline mode - use existing logic
        if (partnerLastActiveTimestamp) {
          const partnerLastActiveMillis = partnerLastActiveTimestamp.toMillis();
          const timeSinceLastActive = Date.now() - partnerLastActiveMillis;

          if (timeSinceLastActive > INACTIVITY_THRESHOLD) {
            showInactivityModal = true;
          }
        } else {
          const timeSinceChatStart = Date.now() - chatStartTime;
          if (timeSinceChatStart > INACTIVITY_THRESHOLD) {
            showInactivityModal = true;
          }
        }
      }
    }
  };

  // Update local "now" every second to recalculate remaining time
  onMount(() => {
    removeUserFromLobby(prolificId);

    notifyPartnerTyping($groupStore["groupId"], prolificId, false);
    notifyPartnerTyping($groupStore["groupId"], partnerProlificId, false);

    interval = setInterval(() => {
      now = Date.now();
      // console.log("remainingSeconds", remainingSeconds);
      if (remainingSeconds <= 0) {
        clearInterval(interval);
        console.log("timer finished");
        dispatch("finished");
        return;
      }
    }, 1000);

    // Check immediately on mount (handles refresh cases)
    setTimeout(checkInactivity, 2000); // Small delay to let messages load

    // Check for partner inactivity at configured interval
    inactivityCheckInterval = setInterval(
      checkInactivity,
      globalVars.inactivityCheckInterval * 1000
    );
  });

  onDestroy(() => {
    clearInterval(interval);
    if (inactivityCheckInterval) {
      clearInterval(inactivityCheckInterval);
    }
  });

  // initialize messages from store -- reactive
  let messages = $derived($groupMessagesStore || []);

  // initialize chat message object
  // WM: using $state to make reactive
  let message = $state({
    absolute_timestamp: serverTime,
  });

  // display to partner when user is typing
  let typingTimer;
  const handleInput = () => {
    // Call util function to notify server that user is typing
    notifyPartnerTyping($groupStore["groupId"], prolificId, true);

    // Clear previous timer
    clearTimeout(typingTimer);

    // Set a new timer to detect when the user stops typing
    typingTimer = setTimeout(() => {
      notifyPartnerTyping($groupStore["groupId"], prolificId, false);
    }, 500);
  };

  // Inactivity detection
  let partnerLastActiveTimestamp = $state(null); // Store the server timestamp
  let showInactivityModal = $state(false);
  let hasRespondedToModal = $state(false); // Track if user has already responded
  let showExitButton = $state(false); // Show persistent exit button after first modal
  let inactivityCheckInterval;
  const INACTIVITY_THRESHOLD =
    globalVars.partnerInactivityThreshold * 60 * 1000; // Convert minutes to milliseconds

  // Update partner's last active time when they send a message
  $effect(() => {
    if (messages && messages.length > 0) {
      const partnerMessages = messages.filter(
        (m) => m.author !== prolificId && m.author !== "Server"
      );
      if (partnerMessages.length > 0) {
        // Get the timestamp of the most recent partner message
        const latestPartnerMessage =
          partnerMessages[partnerMessages.length - 1];
        if (latestPartnerMessage.absolute_timestamp) {
          partnerLastActiveTimestamp = latestPartnerMessage.absolute_timestamp;
        }
      }
    }
  });

  // Reset inactivity modal when phase changes in questionPrompting mode
  $effect(() => {
    if (taskVer === "questionPrompting" && currentPhase) {
      // When phase changes to selecting, both partners consented - reset all inactivity state
      if (currentPhase === "selecting") {
        showInactivityModal = false;
        hasRespondedToModal = false;
        showExitButton = false;
        // Reset the timestamp so inactivity timer starts fresh
        partnerLastActiveTimestamp = Date.now();
      }
    }
  });

  // Reset inactivity tracking when partner sends a new message
  // This allows the modal to show again if partner goes inactive after sending a message
  let lastKnownPartnerMessageCount = $state(0);
  $effect(() => {
    if (taskVer === "questionPrompting" && messages) {
      const partnerMessages = messages.filter(
        (m) => m.author !== prolificId && m.author !== "Server"
      );
      const currentCount = partnerMessages.length;

      // If partner sent a new message, reset the modal response tracking
      if (
        currentCount > lastKnownPartnerMessageCount &&
        lastKnownPartnerMessageCount > 0
      ) {
        hasRespondedToModal = false;
        showInactivityModal = false;
      }
      lastKnownPartnerMessageCount = currentCount;
    }
  });

  // Handle early exit due to inactive partner
  const handleEarlyExit = async () => {
    const success = await handlePartnerInactiveExit(
      prolificId,
      partnerProlificId,
      $groupStore["groupId"]
    );

    if (success) {
      // Force a refresh of the user store to trigger navigation
      userStore.update((state) => ({
        ...state,
        currentState: "partner-survey-complete",
      }));
    }
  };
</script>

<div class="chat-layout">
  {#if taskVer === "questionPrompting"}
    <div class="question-bank-column">
      <!-- Question bank -->
      <QuestionBank
        groupID={$groupStore["groupId"]}
        {prolificId}
        {questions}
        phase={currentPhase}
        {isAsker}
      />
    </div>
  {/if}
  <!-- Chat column -->
  <div class="chat-column">
    <!-- Progress Bar -->
    <div class="relative w-full bar-container">
      <Progressbar
        progress={barProgress}
        text-base
        animate
        precision={2}
        easing={sineOut}
        classes={{
          label:
            (remainingSeconds <= secondsAfterWarning
              ? "bg-red-400"
              : "bg-teal-500") + " text-white",
        }}
        size="h-6"
      />
      {#if taskVer === "questionPrompting"}
        <!-- Tick Marks -->
        <div class="absolute inset-0 pointer-events-none ticks">
          {#each Array(nQuestions) as _, i (i)}
            <!-- Tick line (skip the very first one at i = 0 if you want) -->
            {#if i !== 0}
              <div
                class="absolute top-0 w-0.5 h-full bg-white tick"
                style="left: {(i * 100) / nQuestions}%"
              ></div>
            {/if}

            <!-- Label for the section -->
            <div
              class="absolute top-0 tick-label"
              style="
                left: {((i + 0.5) * 100) / nQuestions}%;
                transform: translateX(-50%);
                font-size: 0.75em;
              "
            >
              Q{i + 1}
            </div>
          {/each}
        </div>
      {/if}
    </div>

    <!-- Visual countdown display -->
    <p
      class="mt-1 text-base font-semibold text-center {remainingSeconds <=
      secondsAfterWarning
        ? 'text-red-500'
        : 'text-black'}"
    >
      Time Left: {timerMinutes}:{timerSeconds < 10
        ? `0${timerSeconds}`
        : timerSeconds}
    </p>

    <!-- Debug and exit button -->
    <div class="text-lg text-center">
      {#if DEBUG}
        <p class="debug-mode-warning" style="color: red;">
          DEBUG MODE: chat timer set to {chatDuration} minute(s)
        </p>
      {/if}

      {#if showExitButton}
        <button
          class="px-3 py-1 mt-2 text-sm text-gray-700 bg-gray-200 rounded transition-colors hover:bg-gray-300"
          onclick={() => (showInactivityModal = true)}
        >
          Partner inactive? Exit early
        </button>
      {/if}
    </div>

    <!-- Chat window -->
    <div id="chatWindow">
      <ul id="messages" class="p-0 list-none">
        <div class="my-4 text-xs text-center text-gray-500">
          You and your partner have joined the chat
        </div>
        {#if Object.keys(messages).length > 0}
          {#each messages as message}
            {#if message.author === prolificId}
              <!-- User message styling -->
              <div class="flex justify-end mb-1">
                <div
                  class="inline-block px-4 py-2 ml-16 max-w-xs text-left text-white rounded-2xl rounded-br-sm"
                  style="background-color: #8b5cf6;"
                >
                  {message.message_string}
                </div>
              </div>
              {#if DEBUG}
                <div class="text-[10px] mr-2 text-right text-gray-400">
                  {formatRelativeTimestamp(message.absolute_timestamp)}
                </div>
              {/if}
            {:else if message.author === "Server"}
              <!-- Server message styling -->
              <div class="my-2 text-xs text-center text-gray-500">
                {message.message_string}
              </div>
            {:else}
              <!-- Other messages styling -->
              <div class="flex justify-start mb-1">
                <div
                  class="inline-block px-4 py-2 mr-16 max-w-xs text-left text-white rounded-2xl rounded-bl-sm"
                  style="background-color: #ec4899;"
                >
                  {message.message_string}
                </div>
              </div>
              {#if DEBUG}
                <div class="text-[10px] ml-2 text-gray-400">
                  {formatRelativeTimestamp(message.absolute_timestamp)}
                </div>
              {/if}
            {/if}
          {/each}
        {/if}
      </ul>
    </div>

    <!-- Input form -->
    <!-- WM: onsubmit to prevent form submission/reset upon pressing enter to submit msg-->
    <form
      onsubmit={(e) => {
        e.preventDefault();
        handleSubmit();
        console.log("form submitted");
      }}
    >
      <div class="flex items-center w-full">
        {#if taskVer === "questionPrompting" && isAsker && (currentPhase === "selecting" || currentPhase === "waiting_for_next")}
          <input
            disabled
            id="m"
            class="flex-1 disabled-input"
            autocomplete="off"
            placeholder="Messaging disabled while choosing question..."
          />
        {:else if taskVer === "questionPrompting" && !isAsker && (currentPhase === "selecting" || currentPhase === "waiting_for_next")}
          <input
            disabled
            id="m"
            class="flex-1"
            autocomplete="off"
            placeholder="Messaging disabled while partner chooses a question..."
          />
        {:else}
          <input
            id="m"
            class="flex-1"
            autocomplete="off"
            placeholder="Message your partner..."
            oninput={handleInput}
            bind:value={message.message_string}
          />
        {/if}
        <!-- WM: specifically type="button" to avoid form submission/reset upon clicking send button-->
        <button type="button" onclick={handleSubmit} aria-label="Send message">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="24"
            height="24"
            viewBox="0 0 24 24"
            fill="none"
            stroke="currentColor"
            stroke-width="2"
            stroke-linecap="round"
            stroke-linejoin="round"
            class="feather feather-send"
            style="width: 20px; height: 20px;"
          >
            <line x1="22" y1="2" x2="11" y2="13" />
            <polygon points="22 2 15 22 11 13 2 9 22 2" />
          </svg>
        </button>
      </div>
    </form>
    <div>
      {#if $groupStore["typing"][partnerProlificId] == true}
        <p class="mt-2 text-sm italic text-center text-gray-500">
          Your partner is typing...
        </p>
      {:else}
        <p class="mt-2 text-sm text-center text-transparent">&nbsp;</p>
      {/if}
    </div>
  </div>
</div>

<!-- Inactivity Modal -->
{#if showInactivityModal}
  <div
    class="fixed top-1/2 left-1/2 z-50 transform -translate-x-1/2 -translate-y-1/2"
  >
    <div
      class="p-6 max-w-md bg-white rounded-lg border border-gray-300 shadow-2xl"
    >
      <h3 class="mb-4 text-lg font-semibold text-center">Partner Inactive</h3>
      <p class="mb-6 text-center">
        Your partner appears to be inactive. Would you like to proceed to the
        next step?
      </p>
      <div class="flex gap-4 justify-center">
        <button
          class="px-6 py-2 bg-gray-300 rounded transition-colors hover:bg-gray-400"
          onclick={() => {
            showInactivityModal = false;
            hasRespondedToModal = true;
            showExitButton = true;
          }}
        >
          Keep Waiting
        </button>
        <button
          class="px-6 py-2 text-white bg-purple-500 rounded transition-colors hover:bg-purple-600"
          onclick={handleEarlyExit}
        >
          Yes, Proceed
        </button>
      </div>
    </div>
  </div>
{/if}

<!-- Chat End -->

<style>
  :root {
    --chat-bg-color: #f3f3f3;
    --author-bg-color: #8b5cf6; /* bg-violet-500 */
    --author-fg-color: #fff;
    --other-bg-color: #f9a8d4; /* bg-pink-300 */
    --other-fg-color: #000;
    --input-bg-color: #fff;
  }

  form {
    background: transparent;
    padding: 0.5rem 0;
    width: 100%;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  form input {
    background: var(--input-bg-color);
    border: 1px solid #e0e0e0;
    padding: 0.75rem 1rem;
    width: 100%;
    color: #000;
    font-size: 1rem;
    border-radius: 1.5rem;
    outline: none;
    transition: border-color 0.2s;
  }

  form input:focus {
    border-color: #8b5cf6; /* bg-violet-500 */
  }

  form button {
    background: #8b5cf6; /* bg-violet-500 */
    border: none;
    padding: 0.5rem;
    color: #fff;
    text-align: center;
    border-radius: 50%;
    width: 2.5rem;
    height: 2.5rem;
    display: flex;
    align-items: center;
    justify-content: center;
    transition: background-color 0.2s;
  }

  form button:hover {
    background: #7c3aed; /* bg-violet-600 */
  }

  form button:active {
    transform: scale(0.95);
  }

  #chatWindow {
    flex: 1 1 auto;
    min-height: 200px;
    width: 100%;
    background-color: var(--chat-bg-color);
    overflow-y: auto;
    overflow-x: hidden;
    border-radius: 1rem;
    padding: 0.5rem;
  }

  #messages {
    align-self: center;
    list-style-type: none;
    margin: 0;
    padding: 0.5rem;
    font-size: 1rem;
  }

  ::placeholder {
    color: #999;
  }

  /* Responsive placeholder for disabled input */
  .disabled-input::placeholder {
    font-size: clamp(0.75rem, 2.5vw, 1rem);
    white-space: nowrap;
    overflow: visible;
  }

  .bar-container {
    position: relative;
    display: inline-block; /* ensures ticks stick to bar width */
  }

  .ticks {
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    pointer-events: none;
  }

  .tick {
    position: absolute;
    top: 0;
    width: 2px;
    height: 100%;
    background-color: white;
    transform: translateX(-50%);
  }

  /* Responsive layout for chat + question bank */
  .chat-layout {
    display: flex;
    flex-direction: row;
    gap: 1rem;
    width: 100%;
    max-width: 100vw;
    height: 100vh;
    padding: 1rem 1rem 1.5rem 1rem;
    box-sizing: border-box;
    overflow: hidden;
  }

  .question-bank-column {
    flex: 0 0 auto;
    width: 420px;
    min-width: 280px;
    max-width: 450px;
    display: flex;
    flex-direction: column;
    height: 100%;
  }

  .chat-column {
    flex: 1 1 auto;
    min-width: 500px;
    max-width: 700px;
    width: 600px; /* Set a consistent starting width to prevent size changes */
    padding: 0 0.5rem;
    display: flex;
    flex-direction: column;
    height: 100%;
  }

  /* Stack vertically on narrow screens */
  @media (max-width: 768px) {
    .chat-layout {
      flex-direction: column;
      align-items: center;
      height: auto;
      min-height: 100vh;
    }

    .question-bank-column,
    .chat-column {
      width: 100%;
      max-width: 100%;
      min-width: auto;
      height: auto;
    }

    .disabled-input::placeholder {
      font-size: 0.5rem;
    }
  }

  /* Very narrow screens - use even smaller font */
  @media (max-width: 480px) {
    .disabled-input::placeholder {
      font-size: 0.5rem;
    }
  }
</style>
