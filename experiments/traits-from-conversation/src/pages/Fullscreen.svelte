<script>
    import Button from '../components/Button.svelte';
    import { DEBUG } from '../globals';
    import { createEventDispatcher} from "svelte";


    const dispatch = createEventDispatcher();
    let fullscreenComplete = $state(false); // state variable for when fullscreen is complete; disables Enter Fullscreen button while DB transaction is in process

    const userRequestFullscreen = async () => {
        if (!DEBUG) {
            await document.documentElement.requestFullscreen();
        }
    };

    let fullscreenHTML = "<div class='prevent-select bounding-div'> \
            <p>Let's get started!</p> \
            <p>The experiment will switch to fullscreen mode when you press the button below.</p> \
        </div>";

</script>

<div class='prevent-select bounding-div'>
    {#if DEBUG}
    <p class="debug-mode-warning" style="color: red;">DEBUG MODE: fullscreen disabled</p>
    {/if}

    <div class="fullscreen-text" style='max-width: 1200px;'>
        {@html fullscreenHTML}
    </div>

    <Button disabled={fullscreenComplete} on:click={() => {
        // set fullscreenComplete to true so we disable the Enter Fullscreen button while making state updates
        fullscreenComplete = true;
        userRequestFullscreen();
        dispatch("finished");
    }}>Enter Fullscreen</Button>
</div>