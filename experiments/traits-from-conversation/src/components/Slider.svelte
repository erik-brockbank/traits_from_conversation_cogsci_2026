<script>
    export let slider_text;
    export let header;
    export let min;
    export let max;
    export let value;
    export let questionCounter;

    function handleMouseDown(event) {
      // set slider thumb opacity to 1 (visible)
      document.documentElement.style.setProperty('--slider-thumb-visibility', '1');
    }
    function handleMouseUp(event) {
      // update slider value
      value = event.target.value;
      // console.log('value udpate: ', value);
      return value
    }
</script>

<style>
  .slider-container {
    display: flex;
    flex-direction: column;
    gap: .5rem;
    margin-left: auto;
    margin-right: auto;
    margin-bottom: 25px;
  }

  .slider-labels {
    display: flex;
    justify-content: space-between;
  }

  input[type="range"] {
    width: 800px;
    appearance: none;
    height: 16px;
    background: #ddd;
    border-radius: 5px;
    outline: none;
    margin-top: 25px;
    cursor: pointer;
  }

  input[type="range"]::-webkit-slider-thumb {
    -webkit-appearance: none;
    appearance: none;
    width: 24px;
    height: 24px;
    background: #c27aff;
    border-radius: 50%;
    /* Set opacity based on `slider-thumb-visibility` variable so it can be hidden whenever slider is reset*/
    opacity: var(--slider-thumb-visibility, 0);
  }

</style>

<div class="slider-container">
  <div class="question-index-header">
    {@html questionCounter}
  </div>
  <div class="question-header">
    {@html header}
  </div>
  <div class="question-text">
    {@html slider_text}
  </div>
  <div class="slider-input">
    <input required type="range" min=0 max=100 on:mousedown={handleMouseDown} on:mouseup={handleMouseUp} />
    <div class="slider-labels">
      <span class="min-label">{min}</span>
      <span class="max-label">{max}</span>
    </div>
  </div>
</div>

