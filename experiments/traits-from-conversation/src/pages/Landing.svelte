<script>
    import Button from '../components/Button.svelte';
    import { createEventDispatcher} from "svelte";



    const dispatch = createEventDispatcher();
    let landingComplete = $state(false); // state variable for when landing is complete; disables Get Started button while DB transaction is in process

    let landingPageHTML = "<div class='prevent-select bounding-div'> \
            <p><u>Welcome!</u></p> \
        </div> \
        <div class='prevent-select bounding-div'> \
            <p>In this experiment, your task is to get to know another person by having a short conversation with them.</p> \
            <p>We expect this study to take approximately 20 minutes to complete, including the time it takes to read these instructions.</p> \
        </div>";

</script>

<div class='prevent-select bounding-div'>
    <div class="landing-page-container" style='max-width: 1200px;'>
        {@html landingPageHTML}
    </div>

    <Button disabled={landingComplete} on:click={() => {
        // set landingComplete to true so we disable the Get Started button while making state updates
        landingComplete = true;
        dispatch("finished");
    }}>Get Started</Button>
</div>