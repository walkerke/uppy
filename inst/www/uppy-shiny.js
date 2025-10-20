// Shiny input binding for Uppy file uploads
// This processes base64 file data and returns a fileInput-compatible dataframe

(function() {

  // Helper function to prepare files for R processing
  function processUppyFiles(rawData) {
    if (!rawData || !rawData.files || !Array.isArray(rawData.files)) {
      return null;
    }

    // Return array marked for custom input handler
    var files = rawData.files.map(function(file, index) {
      return {
        name: file.name,
        size: file.size,
        type: file.type,
        datapath: file.data  // Keep base64 - R input handler will decode it
      };
    });

    // Mark with custom type for Shiny input handler
    return {
      value: files,
      _type: 'uppy.files'
    };
  }

  var uppyInputBinding = new Shiny.InputBinding();

  $.extend(uppyInputBinding, {

    find: function(scope) {
      return $(scope).find('.uppy-input-container');
    },

    getId: function(el) {
      return el.id;
    },

    getValue: function(el) {
      // Get raw data from element
      var rawData = $(el).data('uppy-raw-data');
      if (!rawData) {
        return null;
      }

      // Process and return as fileInput-compatible structure
      return processUppyFiles(rawData);
    },

    setValue: function(el, value) {
      $(el).data('uppy-raw-data', value);
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
        $(el).data('uppy-raw-data', null);
        $(el).trigger('change');
      }
    }

  });

  Shiny.inputBindings.register(uppyInputBinding, 'uppy.uppyInput');

  // Custom message handler for clearing Uppy
  Shiny.addCustomMessageHandler('clearUppy', function(message) {
    if (window[message.instance]) {
      var uppy = window[message.instance];

      // Clear all files from Uppy
      var files = uppy.getFiles();
      files.forEach(function(file) {
        uppy.removeFile(file.id);
      });

      // Also call reset to be thorough
      uppy.reset();

      console.log('Uppy cleared:', message.instance);
    }
  });

  // Custom message handler to clear Shiny input value
  Shiny.addCustomMessageHandler('clearUppyInput', function(message) {
    var el = document.getElementById(message.inputId);
    if (el) {
      $(el).data('uppy-raw-data', null);
      $(el).trigger('change');
      console.log('Uppy input cleared:', message.inputId);
    }
  });

})();
