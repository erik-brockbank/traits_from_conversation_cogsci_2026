<script>
    import Button from '../components/Button.svelte';
    import { createEventDispatcher} from "svelte";
    import ReturnStudy from "./ReturnStudy.svelte";


    const dispatch = createEventDispatcher();
    let consentComplete = $state(false); // state variable for when consent is complete; disables I agree button while DB transaction is in process
    let consentFail = $state(false);

    let consentHtml = "<div class='prevent-select bounding-div'> \
            <p id='legal'><u>Consent to Participate</u></p> \
        </div> \
        <div class='prevent-select bounding-div'> \
            <div class='consent-text'> \
                <p id='legal'>By completing this study, you are participating in research \
                    being performed by cognitive scientists in the Stanford University \
                    Department of Psychology. The purpose of this research is to find out \
                    how people learn about each other using language and conversation. \
                    You must be at least 18 years old to participate. There are neither \
                    specific benefits nor anticipated risks associated with participation \
                    in this study. Your participation in this study is completely voluntary \
                    and you can withdraw at any time by simply exiting the study. You may \
                    decline to answer any or all of the following questions. Choosing not \
                    to participate or withdrawing will result in no penalty. Your anonymity \
                    is assured; the researchers who have requested your participation will \
                    not receive any personal information about you, and any information you \
                    provide will not be shared in association with any personally identifying information. \
                </p> \
                <p>If you have questions about this research, please contact the researchers by sending \
                    an email to \
                    <b><a href='mailto://ebrockbank@stanford.edu'>ebrockbank@stanford.edu</a></b>. \
                    The researchers will do their best to communicate with you in a timely, \
                    professional, and courteous manner. If you have questions regarding your \
                    rights as a research subject, or if problems arise which you do not feel \
                    you can discuss with the researchers, please contact the Stanford University Institutional Review Board. \
                </p> \
            </div> \
        </div> \
        <div class='prevent-select bounding-div'> \
            <p>Click I agree to continue participating in this study.</p> \
        </div>";

  const returnStudy = async () => {
    consentFail = true;
    dispatch("reject");
    console.log('dispatched')
  };
</script>

{#if consentFail}
  <ReturnStudy />
{:else}
<div class='prevent-select bounding-div'>
    <div class="consent-container" style='max-width: 1200px;'>
        {@html consentHtml}
    </div>

    <Button on:click={returnStudy} color={"red"}
        >I do not consent
    </Button>

    <Button color={"green"} disabled={consentComplete} on:click={() => {
        // set consentComplete to true so we disable the I agree button while making state updates
        consentComplete = true;

        dispatch("finished");
    }}>I consent</Button>
</div>
{/if}