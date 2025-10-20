// Shiny input binding for Uppy file uploads
(function() {

  var uppyInputBinding = new Shiny.InputBinding();

  $.extend(uppyInputBinding, {

    find: function(scope) {
      return $(scope).find('.uppy-input-container');
    },

    getId: function(el) {
      return el.id;
    },

    getValue: function(el) {
      // Return uploaded files in fileInput-compatible format
      var data = $(el).data('uppy-files');
      return data || null;
    },

    setValue: function(el, value) {
      $(el).data('uppy-files', value);
    },

    subscribe: function(el, callback) {
      $(el).on('change.uppyInputBinding', function(e) {
        callback();
      });
    },

    unsubscribe: function(el) {
      $(el).off('.uppyInputBinding');
    },

    receiveMessage: function(el, data) {
      // Handle messages from server (e.g., reset)
      if (data.reset) {
        var instanceName = $(el).data('uppy-instance');
        if (window[instanceName]) {
          window[instanceName].reset();
        }
        $(el).data('uppy-files', null);
        $(el).trigger('change');
      }
    }

  });

  Shiny.inputBindings.register(uppyInputBinding, 'uppy.uppyInput');

  // Custom message handler for clearing Uppy
  Shiny.addCustomMessageHandler('clearUppy', function(message) {
    if (window[message.instance]) {
      window[message.instance].reset();
    }
  });

})();
