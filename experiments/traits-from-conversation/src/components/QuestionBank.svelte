<script>
    import Button from './Button.svelte';
    import { submitQuestion, requestNextQuestion } from '../utils.js';
    import { groupStore, questionPromptingConfig } from '../globals';

    const { groupID, prolificId, questions, phase, isAsker } = $props();

    // set nextQuestionButtonDisabled to true by default; is set to false when phase changes to selecting and then set to true after clicking
    let nextQuestionButtonDisabled = $state(true);
    let discussionTimer = null;
    let secondsUntilButtonEnabled = $state(0);
    let countdownInterval = null;

    // Check if current user has already clicked ready
    let userHasClickedReady = $derived(
        $groupStore.questionPrompting?.counter?.includes(prolificId) || false
    );

    $effect(() => {
        // Clear any existing timer
        if (discussionTimer) {
            clearTimeout(discussionTimer);
            discussionTimer = null;
        }

        // Clear countdown interval if exists
        if (countdownInterval) {
            clearInterval(countdownInterval);
            countdownInterval = null;
        }

        // Phase-based button logic
        if (phase === 'discussing') {
            // Initially disable the button when discussion starts
            nextQuestionButtonDisabled = true;
            secondsUntilButtonEnabled = questionPromptingConfig.discussionMinTime;

            // Start countdown
            countdownInterval = setInterval(() => {
                secondsUntilButtonEnabled--;
                if (secondsUntilButtonEnabled <= 0) {
                    clearInterval(countdownInterval);
                    countdownInterval = null;
                }
            }, 1000);

            // Enable button after discussionMinTime (unless user already clicked)
            discussionTimer = setTimeout(() => {
                if (!userHasClickedReady) {
                    nextQuestionButtonDisabled = false;
                    console.log(`Next question button enabled after ${questionPromptingConfig.discussionMinTime} seconds`);
                }
            }, questionPromptingConfig.discussionMinTime * 1000);
        } else if (phase === 'one_ready_for_next') {
            // Disable if user has already clicked, otherwise keep enabled
            nextQuestionButtonDisabled = userHasClickedReady;
        } else {
            // Disable button in all other phases
            nextQuestionButtonDisabled = true;
        }

        // Cleanup function
        return () => {
            if (discussionTimer) {
                clearTimeout(discussionTimer);
            }
            if (countdownInterval) {
                clearInterval(countdownInterval);
            }
        };
    });

    let questionBankEnabled = $derived(isAsker && phase === 'selecting');
    let questionSubmissionButtonDisabled = $state(true);
    let selectedQuestion = $state(null);

    function getInstructionText(phase, isAsker, nextQuestionButtonDisabled, userHasClickedReady, secondsUntilButtonEnabled) {
        if (phase === 'selecting') {
            return isAsker ? "Choose a question from the list below" : "Waiting for your partner to choose a question...";
        }
        if (phase === 'discussing') {
            if (nextQuestionButtonDisabled && secondsUntilButtonEnabled > 0) {
                return `Time to chat! Button enables in ${secondsUntilButtonEnabled} seconds...`;
            }
            return "Time to chat! Click below when ready for the next question.";
        }
        if (phase === 'one_ready_for_next') {
            if (userHasClickedReady) {
                return "Waiting for your partner to be ready for the next question...";
            }
            return "Your partner is ready for the next question. Click when you're ready too!";
        }
        return "";
    }

    let instructionText = $derived(
        getInstructionText(phase, isAsker, nextQuestionButtonDisabled, userHasClickedReady, secondsUntilButtonEnabled)
    );

    function getStatus(q, questionBankEnabled, prolificId) {
        if (questionBankEnabled && q.chosenByProlificId === null) return 'eligible';
        if (q.chosenByProlificId === prolificId) return 'chosen-by-self';
        if (q.chosenByProlificId && q.chosenByProlificId !== prolificId) return 'chosen-by-partner';
        return 'ineligible';
    }

    let questionStatus = $derived(
        questions.map((q) => ({
            ...q,
            status: getStatus(q, questionBankEnabled, prolificId)
        }))
    );

    const handleQuestionClick = (q) => {
        if (q.status === 'eligible') {
            selectedQuestion = q.question_id;
            questionSubmissionButtonDisabled = false;
        }
    }

    const handleQuestionSubmission = () => {
        questionSubmissionButtonDisabled = true;
        submitQuestion(groupID, selectedQuestion, prolificId);
        selectedQuestion = null; // reset selected question
    }

    const handleNextQuestion = () => {
        nextQuestionButtonDisabled = true;
        requestNextQuestion(groupID, prolificId);
    }

</script>


<style>
    .question-bank-wrapper {
        width: 100%;
        height: 100%;
    }

    .question-bank-container {
        width: 100%;
        height: 100%;
        display: flex;
        flex-direction: column;
    }

    .question-bank-header {
        flex-shrink: 0;
    }

    .question-bank-actions {
        flex-shrink: 0;
        padding: 0.5rem 0;
        padding-bottom: 1.75rem; /* Match typing indicator space in chat column */
        text-align: center;
    }

    .question-container {
        margin-top: 10px;
        border-radius: 10px;
    }

    .question-container.eligible {
        background-color: #99F6E4; /* bg-teal-200 */
    }
    .question-container.ineligible {
        background-color: #D1D5DB; /* bg-gray-300 */
    }
    .question-container.chosen-by-self {
        background-color: #DDD6FE; /* bg-violet-200 */
    }
    .question-container.chosen-by-partner {
        background-color: #FBCFE8; /* bg-pink-200 */
    }

    .question-container.eligible:hover {
        background-color: #2DD4BF; /* bg-teal-400 */
    }

    .question-container.is-active {
        transform: scale(1.025);
        box-shadow: 0 10px 20px rgba(0,0,0,0.15);
        background-color: #2DD4BF; /* bg-teal-400 */
    }

    .question-text {
        padding: 10px;
    }

    .question-list-scroll {
        margin-top: 0.5em;
        flex: 1 1 auto;
        min-height: 200px;
        overflow-y: auto;
        background-color: #f3f3f3;
        border-radius: 1rem;
        padding: 0.5rem;
    }

</style>


<div class="question-bank-wrapper">
    <div class="question-bank-container">
        <div class="question-bank-header">
            <span class="text-2xl font-bold">Questions</span>
            <p class="text-med text-gray-500"><em>{instructionText}</em></p>
        </div>
        <div class="question-list-scroll">
            {#each questionStatus as q}
                <div
                    class="question-container {q.status} flex flex-col gap-2"
                    class:is-active={phase === 'selecting' && selectedQuestion === q.question_id && q.status === 'eligible'}
                >
                <button
                    disabled={q.status === 'ineligible'}
                    class="question-text text-sm"
                    onclick={() => handleQuestionClick(q)}
                >
                    {q.question}
                </button>
                </div>
            {/each}
        </div>

        <div class="question-bank-actions">
    {#if questionBankEnabled}
        <Button
            color="purple"
            disabled={questionSubmissionButtonDisabled}
            on:click={handleQuestionSubmission}
        >
            Submit Question
        </Button>
    {/if}
    {#if phase === 'discussing' || phase === 'one_ready_for_next'}
        <button
            class="relative px-4 py-2 mt-1 font-bold text-white rounded overflow-hidden transition-all duration-300
                   {nextQuestionButtonDisabled ? 'bg-gray-300 cursor-not-allowed' : 'bg-violet-500 hover:bg-violet-700 cursor-pointer'}"
            disabled={nextQuestionButtonDisabled}
            onclick={handleNextQuestion}
        >
            {#if nextQuestionButtonDisabled && secondsUntilButtonEnabled > 0}
                <!-- Progress fill background -->
                <div
                    class="absolute inset-0 bg-violet-500 transition-transform duration-1000 ease-linear"
                    style="transform: translateX(-{(secondsUntilButtonEnabled / questionPromptingConfig.discussionMinTime) * 100}%)"
                ></div>
            {/if}
            <!-- Button text -->
            <span class="relative z-10">
                Ready for next question
            </span>
        </button>
    {/if}
        </div>
    </div>
</div>