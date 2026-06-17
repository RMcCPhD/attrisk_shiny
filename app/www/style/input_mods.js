
// Placeholders for greyed-out text indicating numeric range
(() => {
  // Config
  const PLACEHOLDERS = {
  // design
  'design_duration':   '12 - 520',
  'design_arms':       '2 - 6',
  'design_n':          '100 - 10,000',
  // conduct
  'conduct_duration':   '12 - 520',
  'conduct_arms':       '2 - 6',
  'conduct_n':          '100 - 10,000',
  'conduct_age':        '18 - 100',
  'conduct_bmi':        '18 - 50',
  'conduct_sbp':        '90 - 200',
  'conduct_hdl':        '1 - 4',
  'conduct_totalc':     '4 - 10',
  'conduct_t2dur':      '0.01 - 20',
  'conduct_hba1c':      '6 - 10',
  'conduct_egfr':       '15 - 120',
  'conduct_conmed':     '0 - 20',
  'conduct_comorbs':    '0 - 10'
  };

  // Apply placeholders (i.e. greyed out text) to each input
  function applyPlaceholders(map) {
    Object.keys(map).forEach(id => {
      const $el = $('#' + id);
      if ($el.length) $el.attr('placeholder', map[id]);
    });
  }
  
  // Initiate application
  $(document).on('shiny:connected', function() {
    applyPlaceholders(PLACEHOLDERS);
  });
  
  function resetInputs(id) {
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
  }
  
  function registerResetHandler(name) {
    const key = '__' + name + 'Registered';
    if (window[key]) return;
    window[key] = true;
    Shiny.addCustomMessageHandler(name, resetInputs);
  }
  
  // Reset user inputs upon reset button activation
  ['resetBox', 'resetBox2', 'resetBox3'].forEach(registerResetHandler);
})();
