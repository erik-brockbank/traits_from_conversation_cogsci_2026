<script>
    import Button from '../components/Button.svelte';
    import Slider from '../components/Slider.svelte';
    import { createEventDispatcher} from "svelte";
    import { saveFeedback } from '../utils.js';
    import { prolificId }  from '../globals.js';

    const dispatch = createEventDispatcher();
    let feedbackComplete = $state(false); // state variable for when feedback is complete; disables Complete Study button while DB transaction is in process

    // initialize empty responses
    let a1 = $state(null);
    let a2 = $state(null);
    let a3 = $state(null);
    let a4 = $state(null);

    // submit responses to feedback + technical questions
    const submitFeedback = async () => {
        const sliderGetToKnowPartner =
        {
            question: 'sliderGetToKnowPartner',
            response: a1
        }
        const sliderPartnerPredictionAccuracy =
        {
            question: 'sliderPartnerPredictionAccuracy',
            response: a2
        }
        const technical =
        {
            question: 'technical',
            response: a3
        }
        const feedback =
        {
            question: 'feedback',
            response: a4
        }

        // send responses to firestore
        if (a1 == null || a2 == null) {
            alert("Please answer the first two questions before continuing.");
            return;
        } else {
            saveFeedback(prolificId, sliderGetToKnowPartner, sliderPartnerPredictionAccuracy, technical, feedback);
            dispatch("finished");
        }
    };

</script>

<div class='prevent-select bounding-div'>
    <div class="feedback-container" style='max-width: 1200px;'>
        <p><b>Please answer the following questions about the experiment:</b></p>

        <div class="prevent-select bounding-div">
            <Slider bind:value={a1}
                questionCounter = ''
                slider_text = '<p>How well do you think you got to know your conversation partner?</p>'
                header = ''
                min = 'not at all'
                max = 'very well'
            />
        </div>

        <div class="prevent-select bounding-div">
            <Slider bind:value={a2}
                questionCounter = ''
                slider_text = '<p>How accurate do you think your predictions about your conversation partner were in the previous task?</p>'
                header = ''
                min = 'not at all accurate'
                max = 'very accurate'
            />
        </div>

        <div class="prevent-select bounding-div">
            <p>Did you encounter any technical issues? If so, can you please describe them?</p>
            <p>
                <textarea id="technical" name="technical" rows="4" cols="80" bind:value={a3}
                class="border border-gray-400 rounded p-3"></textarea>
            </p>
        </div>

        <div class="prevent-select bounding-div">
            <p>Do you have any other suggestions, feedback, or question ideas for the researchers?</p>
            <p>
                <textarea id="feedback" name="feedback" rows="4" cols="80" bind:value={a4}
                class="border border-gray-400 rounded p-3"></textarea>
            </p>
        </div>
    </div>

    <Button disabled={a1 == null || a2 == null || feedbackComplete} on:click={() => {
        // set feedbackComplete to true so we disable the Complete Study button while making state updates
        feedbackComplete = true;
        submitFeedback();
    }}>Complete Study</Button>
</div>