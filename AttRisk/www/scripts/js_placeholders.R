
# Adds greyed out text to bl participant numeric input boxes
# Indicates values should be means and gives range of permitted values
tags$head(
  tags$script(HTML(
  "    
        /* Add greyed out text to numeric input boxes */
        
        $(document).on('shiny:connected', function(){
          $('#duration').attr('placeholder', 'Total (2 - 260)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#sample').attr('placeholder', 'Total (30 - 20,000)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#facilities').attr('placeholder', 'Total (1 - 500)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#arms').attr('placeholder', 'Total (2 - 6)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_age').attr('placeholder', 'Mean (18 - 150)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_bmi').attr('placeholder', 'Mean (18 - 50)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_sbp').attr('placeholder', 'Mean (120 - 140)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_hdl').attr('placeholder', 'Mean (1 - 4)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_total').attr('placeholder', 'Mean (1 - 4)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_t2dur').attr('placeholder', 'Mean (0.01 - 50)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_hba1c').attr('placeholder', 'Mean (6 - 9)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#mean_egfr').attr('placeholder', 'Mean (60 - 120)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#con_med').attr('placeholder', 'Total (0 - 20)');
        });
        
        $(document).on('shiny:connected', function(){
          $('#comorbs').attr('placeholder', 'Total (0 - 20)');
        });
        
        
        /* Create action signalling a reset of inputs for design or conduct */
        
        Shiny.addCustomMessageHandler('resetBox', function(id) {
          const box = document.getElementById(id);
          if (box) {
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
  });
  "
  ))
)