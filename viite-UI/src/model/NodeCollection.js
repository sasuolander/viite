(function (root) {
  root.NodeCollection = function (backend) {
    var me = this;
    var nodes = [];
    var nodesWithAttributes = [];

    this.getNodes = function() {
      return nodes;
    };

    this.setNodes = function(list) {
      nodes = list;
    };

    this.getNodesWithAttributes = function() {
      return nodesWithAttributes;
    };

    this.setNodesWithAttributes = function(list) {
      nodesWithAttributes = list;
    };


    this.getNodesByRoadAttributes = function(roadAttributes) {
      return backend.getNodesByRoadAttributes(roadAttributes, function (result) {
        if (result.success) {
          var searchResult = result.nodes;
          me.setNodesWithAttributes(searchResult);
          eventbus.trigger('nodeSearchTool:fetched', searchResult.length);
        } else {
          applicationModel.removeSpinner();
          new ModalConfirm(result.errorMessage);
        }
      });
    };

    eventbus.on('node:fetched', function(nodes, zoom) {
      me.setNodes(nodes);
      eventbus.trigger('node:addNodesToMap', nodes, zoom);
    });

    eventbus.on('nodeSearchTool:clickNode', function (index, map) {
      var node = nodesWithAttributes[index];
      map.getView().setCenter([node.coordX, node.coordY]);
      map.getView().animate({
        zoom: 12,
        duration: 1500
      });
    });

    eventbus.on('nodeSearchTool:refreshView', function (map) {
      var coords = [];
      _.each(nodesWithAttributes, function(node) {
        coords.push([node.coordX, node.coordY]);
      });
      map.getView().fit(new ol.geom.Polygon([coords]), map.getSize());
    });

  };
})(this);