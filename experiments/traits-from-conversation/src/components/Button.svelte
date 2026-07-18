<script>
  // simple styled reusable button component
  export let color = "purple";
  export let disabled = false;

  let tailwindColor, tailwindHover;
  if (color === "purple") {
    tailwindColor = "bg-violet-500";
    tailwindHover = "hover:bg-violet-700";
  } else if (color === "red") {
    tailwindColor = "bg-red-600";
    tailwindHover = "hover:bg-red-700";
  } else if (color === "green") {
    tailwindColor = "bg-green-600";
    tailwindHover = "hover:bg-green-700";
  } else if (color === "blue") {
    tailwindColor = "bg-blue-500";
    tailwindHover = "hover:bg-blue-700";
  } else if (color === "fuchsia-gradient") {
    tailwindColor = "bg-fuchsia-400";
    tailwindHover = "hover:bg-gradient-to-r from-violet-500 to-fuchsia-500";
    // FOX
  } else if (color == "fox-gradient") {
    tailwindColor = "bg-orange-400";
    tailwindHover = "hover:bg-gradient-to-r from-orange-400 to-orange-700";
    // UNICORN
  } else if (color == "unicorn-gradient") {
    tailwindColor = "bg-purple-400";
    tailwindHover = "hover:bg-gradient-to-r from-purple-400 to-purple-700";
    // OCTOPUS
  } else if (color == "octopus-gradient") {
    tailwindColor = "bg-pink-400";
    tailwindHover = "hover:bg-gradient-to-r from-pink-400 to-pink-700";
    // TIGER
  } else if (color == "tiger-gradient") {
    tailwindColor = "bg-amber-400";
    tailwindHover = "hover:bg-gradient-to-r from-amber-400 to-amber-700";
  }
</script>

<!-- Disable cursor when button is disabled -->
<style>
  button:disabled {
    cursor: not-allowed;
  }
</style>

<button
  class="px-4 py-2 font-bold text-white rounded focus:outline-none
  hover:cursor-pointer m-[25px]
  disabled:bg-gray-300 disabled:text-gray-50
  {tailwindColor} {tailwindHover}"
  disabled={disabled}
  on:click>
  <slot>add text here</slot>
</button>
