<script>
    import Button from '../components/Button.svelte';
    import { createEventDispatcher} from "svelte";
    import { taskVer } from '../globals';

    const dispatch = createEventDispatcher();
    // state variable for when instructions are complete; disables buttons while DB transaction is in process
    let instructionsComplete = $state(false);

    let instructionsIndex = $state(0);
    let { instructionSet } = $props();

    // Define instructions based on taskVer
    const instructionsContent = taskVer === 'questionPrompting' ? {
        // Instructions for self-survey
        'instructions-1': [
            '<p>To begin, we are going to ask you some questions about yourself.</p>\
            <p></br></p>',
        '<p>On the following pages, you will be presented with a series of statements, such as: \
            </br><em>I enjoy going out with friends more than I enjoy staying in.</em></p>',
        '<p>Below each statement will be a slider, with one end labeled <em>Strongly Disagree</em> and the other labeled <em>Strongly Agree</em>.</p> \
            <p>Place the slider at the point that best reflects how much you agree with the statement.</p>',
        '<p>There are no right or wrong answers to these questions; please just answer as honestly as you can.</p>\
            <p>Click the <em>Next</em> button to get started.</p>'
        ],
        // Instructions for chat
        'instructions-2': [
            // 1. Next: lobby for partner matching
            '<p>Awesome! Now, it\'s time to get to know another participant.</p>\
                <p>After these instructions, you will be placed in a Lobby where you will be matched with a conversation partner.</p>\
                <p></br></p>',
            // 2. Matching: possible outcomes
            '<p>If you have not been matched with a partner after <b><em>5 minutes</em></b>, you will be given the option to return to Prolific and be paid for your time.</p>\
                <p>Once you have been matched with a partner, a chat window will appear for both of you to begin the conversation.</p>\
                <p></br></p>',
            // 3. Chat: question bank
            '<p>To guide the conversation, you and your partner will be shown a list of questions.</p>\
                <p>One person will be randomly selected to choose the first question to discuss.</p>\
                <p>After choosing the question, <em>both of you should discuss your answers together</em>.</p>',
            // 4. Question discussion: timing
            '<p>The chat will continue for <b><em>10 minutes</em></b>, and you can spend as long as you want on each question.</p>\
                <p>However, you should shoot for around <b><em>2 minutes per question</em></b> to leave time for multiple questions.</p>\
                <p>At the top of the screen, there will be a progress bar broken out into sections indicating the recommended time for each question.</p>',
            // 5. Ready for next question
            '<p>When you are ready to move on to the next question, click the button that says <em>Ready for next question</em>.</p>\
                <p>The button will appear below the list of questions shortly after you begin chatting.</p>\
                <p>However, you should wait until you finish discussing the current question before clicking it.</p>\
                <p>Once you and your partner have both clicked, one person will choose the next question.</p>',
            // 6. Question refresh
            '<p>Throughout the chat, you and your partner will alternate who chooses the next question.</p>\
                <p>If you discuss all of the questions before time is up, a new set of questions will appear to keep the conversation going.</p>\
                <p>Once the time is up, we will automatically close the chat window.</p>',
            // 7. Click next to get started
            '<p>Click <em>Next</em> to enter the lobby and be matched with your conversation partner.</p>'
        ],
        // Instructions for partner-survey
        'instructions-3': [
            '<p>Great job! We hope you enjoyed getting to know your conversation partner 😊.</p>\
            <p>Now, we are going to ask you some questions about them.</p>',
            '<p>On the following pages, you will be presented with a series of statements, such as: \
                </br><em>I enjoy going out with friends more than I enjoy staying in.</em></p>',
            '<p>Below each statement will be a slider, with one end labeled <em>Strongly Disagree</em> and the other labeled <em>Strongly Agree</em>.</p> \
                <p>Place the slider at the point that best reflects how much you think your <em><b>conversation partner</em></b> would agree with the statement.</p>',
            '<p>There are no right or wrong answers; please answer as best as you can based on what you\'ve learned about your partner.</p>\
                <p>Click the <em>Next</em> button to get started.</p>'
        ],
        // Instructions for demographics + feedback
        'instructions-4': [
            '<p>Nice work! You have now completed the study.</p>\
                <p>On the next page, you will be asked several demographic questions and two questions about your experience in the study.</p>\
                <p>When you are finished, you will be redirected to the Prolific homepage and paid for your time.</p>',
        ]
    } : {
            // Instructions for self-survey
            'instructions-1': [
                '<p>To begin, we are going to ask you some questions about yourself.</p>\
                <p></br></p>',
            '<p>On the following pages, you will be presented with a series of statements, such as: \
                </br><em>I enjoy going out with friends more than I enjoy staying in.</em></p>',
            '<p>Below each statement will be a slider, with one end labeled <em>Strongly Disagree</em> and the other labeled <em>Strongly Agree</em>.</p> \
                <p>Place the slider at the point that best reflects how much you agree with the statement.</p>',
            '<p>There are no right or wrong answers to these questions; please just answer as honestly as you can.</p>\
                <p>Click the <em>Next</em> button to get started.</p>'
            ],
            // Instructions for chat
            // TODO: grab instructions for this part from Aron et al. or more recent fMRI study Robert metioned?
            'instructions-2': [
                '<p>Awesome! Now, it\'s time to meet another participant.</p>\
                    <p>After these instructions, you will be placed in a Lobby where you will be matched with a conversation partner.</p>\
                    <p></br></p>',
                '<p>If you have not been matched with a partner after <b><em>5 minutes</em></b>, you will be given the option to return to Prolific and be paid for your time.</p>\
                    <p>Once you have been matched with a partner, a chat window will appear for both of you to begin the conversation.</p>\
                    <p></br></p>',
                '<p>During this conversation, your goal is simply to <b><i>get to know each other</b></i>.</p>',
                '<p>The chat will continue for <b><em>10 minutes</em></b>.</p>\
                    <p>Once the time is up, we will automatically close the chat window.</p>\
                    <p>Click <em>Next</em> to enter the lobby and be matched with your conversation partner.</p>'
            ],
            // Instructions for partner-survey
            'instructions-3': [
                '<p>Great job! We hope you enjoyed getting to know your conversation partner 😊.</p>\
                <p>Now, we are going to ask you some questions about them.</p>',
                '<p>On the following pages, you will be presented with a series of statements, such as: \
                    </br><em>I enjoy going out with friends more than I enjoy staying in.</em></p>',
                '<p>Below each statement will be a slider, with one end labeled <em>Strongly Disagree</em> and the other labeled <em>Strongly Agree</em>.</p> \
                    <p>Place the slider at the point that best reflects how much you think your <em><b>conversation partner</em></b> would agree with the statement.</p>',
                '<p>There are no right or wrong answers; please answer as best as you can based on what you\'ve learned about your partner.</p>\
                    <p>Click the <em>Next</em> button to get started.</p>'
            ],
            // Instructions for demographics + feedback
            'instructions-4': [
                '<p>Nice work! You have now completed the study.</p>\
                    <p>On the next page, you will be asked several demographic questions and two questions about your experience in the study.</p>\
                    <p>When you are finished, you will be redirected to the Prolific homepage and paid for your time.</p>',
            ]
        };

    let instructionsHtml = $state(instructionsContent);
</script>

<div class='prevent-select bounding-div'>
    <div class="instructions-container" style='max-width: 1200px;'>
        {@html instructionsHtml[instructionSet][instructionsIndex]}
    </div>

    <Button disabled={instructionsIndex == 0 || instructionsComplete} on:click={() => {
        instructionsIndex--;
    }}>Back</Button>
    <Button disabled={instructionsComplete} on:click={() => {
        instructionsIndex++;
        if (instructionsIndex == instructionsHtml[instructionSet].length) {
            // set instructionsComplete to true so we disable the Next and Back buttons while making state updates
            instructionsComplete = true;
            dispatch("finished");
        }
    }}>Next</Button>
</div>