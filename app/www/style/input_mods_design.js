
// Design placeholders
(() => {
  // Config
  const UI_HELPERS_CONFIG = {
    placeholders: {
      'design-duration':    '12 - 520',
      'design-facilities':  '1 - 500',
      'design-arms':        '2 - 6'
    }
  };

  // Apply placeholders (i.e. greyed out text) to each input
  function applyPlaceholders(map) {
    if (!map) return;
    Object.keys(map).forEach(function(id) {
      const $el = $('#' + id);
      if ($el.length) $el.attr('placeholder', map[id]);
    });
  }
  
  // Initiate application
  $(document).on('shiny:connected', function() {
    applyPlaceholders(UI_HELPERS_CONFIG.placeholders);
  });
  
  // Reset design inputs upon restart button activation
  if (!window.__resetBoxRegistered) {
    window.__resetBoxRegistered = true;
    Shiny.addCustomMessageHandler('resetBox', function(id) {
      const box = document.getElementById(id);
      if (!box) return;
      const inputs = box.querySelectorAll('input, select, textarea');
      inputs.forEach(el => {
        if (el.type === 'checkbox' || el.type === 'radio') {
          el.checked = el.defaultChecked;
        } else {
          el.value = el.defaultValue;
        }
        el.dispatchEvent(new Event('change', { bubbles: true }));
      });
    });
  }
})();
