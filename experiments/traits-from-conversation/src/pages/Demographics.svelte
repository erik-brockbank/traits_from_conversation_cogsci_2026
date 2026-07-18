<script>
    import Button from '../components/Button.svelte';
    import { createEventDispatcher, onMount} from "svelte";
    import { saveSurveyResponses, scrollWindowToTop }  from '../utils.js';
    import { prolificId }  from '../globals.js';

    const dispatch = createEventDispatcher();
    let demographicsComplete = $state(false); // state variable for when demographics is complete; disables Next button while DB transaction is in process

    // initialize empty responses
    let a1 = $state(null);
    let a2 = $state(null);
    let a3 = $state(null);

    // submit responses to demographics questions
    const submitDemographics = async () => {
        // responses
        const responses = [
        {
            index: 1,
            question: 'age',
            response: a1
        },
        {
            index: 2,
            question: 'gender',
            response: a2
        },
        {
            index: 3,
            question: 'race',
            response: a3
        }
        ];

        // check for any empty responses
        if (a1 == null || a2 == null || a3 == null) {
            alert("Please answer all questions before continuing.");
            return;
        } else {
            // call function to send data to firestore
            saveSurveyResponses(prolificId, responses);
            dispatch("finished");
        }
    };

    onMount(() => {
        scrollWindowToTop();
    });

</script>

<div class='m-25'>
    <div class='text-center'>
        <p><b>Please answer the following questions about yourself:</b></p>
        <br>
    </div>

    <div class="prevent-select grid grid-cols-[auto_1fr] gap-x-6 gap-y-4 text-left mx-auto w-fit mb-3 items-center">
        <label for="age">Age:</label>
        <input
            id="age"
            name="age"
            type="number"
            min="18"
            bind:value={a1}
            class="border border-gray-400 rounded h-10"
        />

        <label for="gender">Gender:</label>
        <select
            bind:value={a2}
            id="gender"
            name="gender"
            class="border border-gray-400 rounded h-10"
        >
            <option value=null disabled>— Select —</option>
            <option value="Male">Male</option>
            <option value="Female">Female</option>
            <option value="Non-binary">Non-binary</option>
            <option value="Prefer Not to Say">Prefer Not to Say</option>
        </select>

        <label for="race">Race:</label>
        <select
            bind:value={a3}
            id="race"
            name="race"
            class="border border-gray-400 rounded h-10"
        >
            <option selected></option>
            <option value="Black">Black</option>
            <option value="White">White</option>
            <option value="Asian">Asian</option>
            <option value="Indigenous">Indigenous</option>
            <option value="Mixed race">Mixed race</option>
            <option value="Other">Other</option>
            <option value="Prefer Not to Say">Prefer Not to Say</option>
        </select>
    </div>

    <Button disabled={a1 == null || a2 == null || a3 == null || demographicsComplete} on:click={() => {
        // set demographicsComplete to true so we disable the Next button while making state updates
        demographicsComplete = true;
        submitDemographics();
    }}>Next</Button>
</div>