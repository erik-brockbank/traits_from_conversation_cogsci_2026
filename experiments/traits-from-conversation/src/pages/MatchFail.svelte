<!-- MatchFail.svelte

  If the user fails to find a partner after 5 (globalVars.waitLimit) minutes
  (Lobby.svelte), they will be presented with this page.

-->

<script>
  import { onMount, createEventDispatcher } from "svelte";
  import { removeUserFromLobby } from "../utils.js";
  import { prolificId, DEBUG } from "../globals.js";
  import Button from "../components/Button.svelte";
  const dispatch = createEventDispatcher();

  onMount(async () => {
    await removeUserFromLobby(prolificId);
  });


</script>

<div
  class="flex flex-col items-center justify-center h-screen text-black bg-white"
>
  <h1 class="mb-4 text-4xl font-bold">Failed to find a partner</h1>
  {#if DEBUG === false}
    <h1 class="text-xl">👋 Hi there!</h1>
  {:else}
    <h1 class="text-xl">👋 Hi <b>{prolificId}</b>!</h1>
  {/if}
  <br />
  <p class="mb-2 text-lg text-center">
    Unfortunately, we were unable to match you with a partner in the alotted time.
    <br/>There may not be enough workers trying to participate right now.
    <br/><br/>Click <b>Return to Prolific</b> to be redirected to Prolific and automatically receive credit for the study.
    <br/>Click <b>Return to Waiting Room</b> to return to the waiting room and try to match with a partner again
    <br/>(returning to the waiting room will not change your credit for the study).
  </p>
  <div>
    <Button on:click={() => {dispatch("redirect-to-prolific")}}>Return to Prolific</Button>
    <Button on:click={() => {dispatch("redirect-to-lobby")}}>Return to Waiting Room</Button>
  </div>
</div>
