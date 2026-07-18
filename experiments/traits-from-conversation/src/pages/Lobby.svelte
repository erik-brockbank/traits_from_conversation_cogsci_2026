<!--
  Lobby.svelte

      This page is shown to users while they wait for other users to join the
      lobby. It shows how long they have been waiting and how long they will
      need to wait before being redirected (if no other participant joins).
      If the timer runs out, participants redirected to MatchFail.svelte.

-->
<script>
  import { onMount, createEventDispatcher, onDestroy, } from "svelte";
  import { removeUserFromLobby, reqMetaDocChange} from "../utils.js";
  import { prolificId, userStore, globalVars, destroyUserTimer, DEBUG } from "../globals.js";
  const dispatch = createEventDispatcher();

  // waiting room timer
  let counter = $state(0);
  let waitLimit = globalVars.waitLimit

  // these automatically update when `counter`
  // changes, because of the `$:` prefix
  const timerMinutes = $derived(Math.floor(counter / 60));
  const timerMinName = $derived(timerMinutes > 1 ? "mins" : "min");
  const timerSeconds = $derived(counter - timerMinutes * 60);

  // initialize interval
  let interval;

  // Check number of users in lobby on mount
  onMount(async () => {
    // Always start the timer, even if user has a groupId
    // This allows re-matching users if needed
    console.log("starting lobby timer");
    interval = setInterval(() => {
      counter++;
      console.log("counter", counter);
      if (counter == waitLimit * 60) {
        clearInterval(interval);
        // call utils function to remove prolificId from metaStore
        removeUserFromLobby(prolificId);
        dispatch("match-failed");
        return;
      }
    }, 1000);

    // add user to lobby
    await reqMetaDocChange(prolificId);
  });


  // Reactively check if user gets matched
  $effect(() => {
    if ($userStore["groupId"] && $userStore["groupId"] !== "") {
      console.log("user has been matched, dispatching finished");
      clearInterval(interval);
      dispatch("finished");
    }
  });

  // clear timer and interval
  onDestroy(() => {
    removeUserFromLobby(prolificId);
    clearInterval(interval);

    if ($userStore["groupId"] != "") {
      // remove user from group
      destroyUserTimer.set(true);
    }
  });

  // Subscribe to changes in the shouldRemoveUserStore
  const unsubscribe = destroyUserTimer.subscribe((shouldRemove) => {
    if (shouldRemove) {
      clearInterval(interval);
      removeUserFromLobby(prolificId);
    }
  });
</script>

<div class="flex flex-col items-center justify-center h-screen text-black bg-white">

  <!-- TODO consider removing debug message below before running production -->
  {#if DEBUG}
  <p class="debug-mode-warning" style="color: red;">DEBUG MODE: lobby timer set to {waitLimit} minute(s) and greeting below includes prolificID</p>
  {/if}

  <h1 class="items-center justify-center mb-4 text-4xl font-bold">
    Chatroom Lobby
  </h1>
  {#if DEBUG === false}
    <h1 class="text-xl">👋 Hi there!</h1>
  {:else}
    <h1 class="text-xl">👋 Hi <b>{prolificId}</b>!</h1>
  {/if}
  <br />
  <p class="text-left">
    You will wait for a maximum of <span class="font-semibold"
      >{waitLimit} minutes
    </span> to be matched with a partner.
  </p>
  <p class="text-left">
    You have been waiting for approx.
    <span class="font-semibold text-red-500 mins">{timerMinutes}</span
    >{timerMinName}
    <span class="font-semibold text-red-500 secs">{timerSeconds}</span>s
  </p>
</div>


