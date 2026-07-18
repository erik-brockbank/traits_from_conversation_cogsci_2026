<script>
  import { onMount } from 'svelte';
  import { scrollWindowToTop } from '../utils.js';
  export const text = 'Loading...';

  onMount(() => {
    // Scroll the entire document to the top
    scrollWindowToTop();
  });
</script>

<div class='grid h-screen grid-rows-4 text-black bg-white place-items-center'>
  <div
    class='w-32 h-32 row-start-1 border-4 border-solid rounded-full border-fuchsia-400 animate-spin border-t-transparent'
  ></div>
  <div class='row-start-2 text-center'>
    <h1 class='text-6xl'>{text}</h1>
  </div>
</div>
