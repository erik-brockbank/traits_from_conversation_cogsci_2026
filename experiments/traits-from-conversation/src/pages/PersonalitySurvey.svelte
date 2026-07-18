<script>
    import Button from '../components/Button.svelte';
    import Slider from '../components/Slider.svelte';

    import { bigfiveScales, comprehensionCheckScales, prolificId, DEBUG, serverTime } from '../globals';
    import { shuffleArray, saveSliderResponses } from '../utils';
    import { createEventDispatcher} from "svelte";


    const dispatch = createEventDispatcher();
    // state variable for when survey is complete; disables Next button while DB transaction is in process
    let surveyComplete = $state(false);

    let { surveyData } = $props();
    let questions = $state([]);
    // randomize question order (including comprehension checks)
    if (!DEBUG) {
        questions = shuffleArray([...bigfiveScales, ...comprehensionCheckScales]);
    } else {
        questions = shuffleArray([...bigfiveScales, ...comprehensionCheckScales]).slice(0, 5);
    }
    let responses = $state([]);

    // initialize empty score on slider as reactive state
    let sliderValue = $state(null);
    // initialize index describing how many questions person has gone through as reactive state
    let surveyIndex = $state(0);
    // initialize slider thumb visibility to 0 (hidden)
    document.documentElement.style.setProperty('--slider-thumb-visibility', '0');

    const sendSliderResponsesToDB = async (responses) => {
        console.log("asyncDBPush -- prolificId", prolificId);
        console.log("asyncDBPush -- responses", responses);
        let responseKey = surveyData.surveyType === 'self' ? 'self-slider-responses' : 'partner-slider-predictions';
        await saveSliderResponses(prolificId, responseKey, responses);
        dispatch("finished");
    };

    const recordSliderResponse = (questionIdx, questionInfo, response, sendToDB = false) => {
        // console.log('recordSliderResponse -- questionIdx: ', questionIdx);
        // console.log('recordSliderResponse -- questionInfo: ', questionInfo);
        // console.log('recordSliderResponse -- response: ', response);
        responses.push({
            question_idx: questionIdx + 1, // convert to 1-indexed for DB
            question_submit_ts: Date.now(), // add timestamp for end of a given slider
            response: parseInt(response), // convert to integer for DB
            ...questionInfo,
        });
        if (sendToDB) {
            // console.log('sending responses to DB');
            sendSliderResponsesToDB($state.snapshot(responses));
        }
    }

</script>

<div class='prevent-select bounding-div'>
    {#if DEBUG}
    <p class="debug-mode-warning" style="color: red;">DEBUG MODE: showing {questions.length} questions</p>
    {/if}

    <div class="survey-container" style='max-width: 1200px;'>
        <Slider bind:value={sliderValue}
            questionCounter = '<p><u>Question {surveyIndex + 1} of {questions.length}</u></p>'
            slider_text = '<p><b>{questions[surveyIndex].scale_text}.</b></p>'
            header = {
                surveyData.surveyType === 'self' ?
                    '<p>To what extent do <b><em>you</em></b> agree with the following statement?</p>' :
                    '<p>To what extent do you think <b><em>your conversation partner</em></b> would agree with the following statement?</p>'
            }
            min = {questions[surveyIndex].scale_min}
            max = {questions[surveyIndex].scale_max}
        />
    </div>

    <Button disabled={sliderValue == null || surveyComplete} on:click={() => {
        // if questions left, increment survey index by 1; else, dispatch finished event
        if (surveyIndex < questions.length - 1) {
            recordSliderResponse(surveyIndex, questions[surveyIndex], sliderValue);
            surveyIndex++;
            // reset slider value and thumb opacity
            sliderValue = null;
            document.documentElement.style.setProperty('--slider-thumb-visibility', '0');
        }
        else {
            // set surveyComplete to true so we disable the Next button while making state updates
            surveyComplete = true;
            recordSliderResponse(surveyIndex, questions[surveyIndex], sliderValue, true);
        }
    }}>Next</Button>
</div>