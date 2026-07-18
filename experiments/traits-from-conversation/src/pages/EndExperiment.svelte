<script>
    import { DEBUG } from "../globals";
    import Button from '../components/Button.svelte';
    import { createEventDispatcher} from "svelte";


    const dispatch = createEventDispatcher();
    let experimentComplete = $state(false); // state variable for when experiment is complete; disables Finish button while DB transaction is in process

    let completionHTML = "<div class='prevent-select bounding-div'> \
            <p>Congrats! You are all done. Thanks for participating in our study!</p> \
            <p>Click Finish to submit this study to Prolific.</p> \
            <p> After you click Finish, you will see a blank page on this web page \
                but will be redirected to the Prolific homepage. \
            </p> \
            <p>This means that your participation has been logged.<\p> \
            <p>If you do not receive credit immediately, please wait a few days. \
        </div>";

</script>

<div class='prevent-select bounding-div'>

    <div class="completion-container" style='max-width: 1200px;'>
        {@html completionHTML}
    </div>

    <Button disabled={experimentComplete} on:click={() => {
        // set experimentComplete to true so we disable the Finish button while making state updates
        experimentComplete = true;
        dispatch("finished");
    }}>Finish</Button>
</div>