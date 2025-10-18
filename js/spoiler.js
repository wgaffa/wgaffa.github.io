document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll(".spoiler").forEach(pre => {
    // Create wrapper
    const wrapper = document.createElement("div");
    wrapper.classList.add("spoiler-wrap");

    pre.classList.forEach(c => {
        if (c !== "spoiler") wrapper.classList.add(c);
    });

    // Create button
    const btn = document.createElement("button");
    btn.className = "spoiler-toggle f6 link dim br2 ph2 pv1 mb2 bg-dark-gray near-white";
    const labelShow = "Show " + (pre.dataset.label || "Spoiler")
    const labelHide = "Hide " + (pre.dataset.label || "Spoiler")
    btn.textContent = labelShow;

    // Hide by default
    pre.classList.add("dn");

    // Insert
    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.appendChild(btn);
    wrapper.appendChild(pre);

    // Toggle behavior
    btn.addEventListener("click", () => {
      if (pre.classList.contains("dn")) {
        pre.classList.remove("dn"); pre.classList.add("db");
        btn.textContent = labelHide;
      } else {
        pre.classList.remove("db"); pre.classList.add("dn");
        btn.textContent = labelShow;
      }
    });
  });
});
