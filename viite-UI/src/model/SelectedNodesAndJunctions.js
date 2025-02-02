(function (root) {
  root.SelectedNodesAndJunctions = function (nodeCollection) {
    var current = {};

    var openNode = function (node, openNodetemplates) {
      clean();
      setCurrentNode(node);
      eventbus.trigger('node:selected', current.node, openNodetemplates);
    };

    var getCurrentNode = function () {
      return current.node;
    };

    var setCurrentNode = function (node) {
      current.node = _.cloneDeep(node);
    };

    var templates = function (coordinates) {
      return {
        nodePoints: nodeCollection.getNodePointTemplatesByCoordinates(coordinates),
        junctions: nodeCollection.getJunctionTemplateByCoordinates(coordinates)
      };
    };

    var openNodePointTemplate = function (nodePointTemplate) {
      openTemplates(templates(nodePointTemplate.coordinates));
    };

    var openJunctionTemplate = function (junctionTemplate) {
      openTemplates(templates(_.first(junctionTemplate.junctionPoints).coordinates));
    };

    var openTemplates = function (templatesToOpen) {
      clean();
      setCurrentTemplates(templatesToOpen.nodePoints, templatesToOpen.junctions);
      eventbus.trigger('templates:selected', current.templates);
    };

    var getCurrentTemplates = function () {
      return current.templates;
    };

    var setCurrentTemplates = function (nodePoints, junctions) {
      current.templates = {
        nodePoints: _.cloneDeep(nodePoints),
        junctions: _.cloneDeep(junctions)
      };
    };

    var getJunctions = function () {
      return nodeCollection.getNodeByNodeNumber(current.node.nodeNumber).junctions;
    };

    var getNodePoints = function () {
      return nodeCollection.getNodeByNodeNumber(current.node.nodeNumber).nodePoints;
    };

    var addNodePointTemplates = function (nodePoints) {
      _.each(nodePoints, function (nodePoint) {
        current.node.nodePoints.push(nodePoint);
      });
      eventbus.trigger('nodePointTemplates:selected', {nodePoints: nodePoints});
    };

    var addJunctionTemplates = function (junctions) {
      _.each(junctions, function (junction) {
        current.node.junctions.push(junction);
      });
      eventbus.trigger('junction:validate');
      eventbus.trigger('junctionTemplates:selected', {junctions: junctions});
    };

    var getStartingCoordinates = function () {
      return current.node.startingCoordinates;
    };

    var setStartingCoordinates = function (coordinates) {
      current.node.startingCoordinates = coordinates;
    };

    var setCoordinates = function (coordinates) {
      current.node.coordinates = coordinates;
      eventbus.trigger('change:node-coordinates');
    };

    var setNodeName = function (name) {
      current.node.name = name;
      updateNodesAndJunctionsMarker();
    };

    var setNodeType = function (type) {
      current.node.type = type;
      updateNodesAndJunctionsMarker();
    };

    var setStartDate = function (startDate) {
      current.node.startDate = startDate;
    };

    var typeHasChanged = function (nodeType) {
      if (current.node.nodeNumber) {
        return nodeCollection.getNodeByNodeNumber(current.node.nodeNumber).type !== nodeType;
      } else return LinkValues.NodeType.UnknownNodeType.value !== nodeType;
    };

    var getInitialStartDate = function () {
      return nodeCollection.getNodeByNodeNumber(current.node.nodeNumber).startDate;
    };

    var setJunctionNumber = function (id, junctionNumber) {
      var junction = _.find(current.node.junctions, function (junctionToSet) {
        return junctionToSet.id === id;
      });

      if (!_.isUndefined(junction)) {
        junction.junctionNumber = junctionNumber;
        eventbus.trigger('junction:validate');
        updateNodesAndJunctionsMarker();
      }
    };

    var getJunctionPoint = function (id) {
      var junctionPoints = _.flatMap(current.node.junctions, function (junction) {
        return junction.junctionPoints;
      });
      return _.find(junctionPoints, function (jp) {
        return jp.id === id;
      });
    };

    var setJunctionPointAddress = function (idString, addr) {
      var ids = idString.split("-");
      if (ids.length === 2) {
        _.each(ids, function (id) {
          var jp = getJunctionPoint(parseInt(id));
          if (_.isUndefined(jp)) {
            console.log("Failed to find junction point " + id + " and set it's address to " + addr + ".");
          } else {
            jp.addrM = addr;
          }
        });
        eventbus.trigger('junctionPoint:validate', idString, addr);
      } else {
        console.log("Failed to update junction point address. (ids: " + idString + ", address: " + addr + ")");
      }
    };

    var detachJunctionAndNodePoints = function (junction, nodePoints) {
      if (!_.isUndefined(junction)) {
        _.remove(current.node.junctions, function (j) {
          return j.id === junction.id;
        });
        eventbus.trigger('junction:detach', junction);
      }
      _.each(nodePoints, function (nodePoint) {
        _.remove(current.node.nodePoints, function (np) {
          return np.id === nodePoint.id;
        });
        eventbus.trigger('nodePoint:detach', nodePoint);
      });
    };

    var attachJunctionAndNodePoints = function (junction, nodePoints) {
      if (!_.isUndefined(junction)) {
        if (_.filter(current.node.junctions, function (j) {
          return j.id === junction.id;
        }).length === 0) {
          current.node.junctions.push(junction);
          eventbus.trigger('junction:attach', junction);
        }
      }
      _.each(nodePoints, function (nodePoint) {
        if (_.filter(current.node.nodePoints, function (np) {
          return np.id === nodePoint.id;
        }).length === 0) {
          current.node.nodePoints.push(nodePoint);
          eventbus.trigger('nodePoint:attach', nodePoint);
        }
      });
    };

    var validateJunctionNumbers = function () {

      var errorMessage = function (junctions) {
        var message = '';

        if (junctions.length !== 1) {
          message = 'Liittymänumero on jo käytössä'; // junction number is already in use
        } else if (_.isNaN(_.first(junctions).junctionNumber) || !_.isEmpty(_.find(junctions, function (j) {
          return j.junctionNumber <= 0;
        }))) {
          message = 'Liittymänumero on pakollinen tieto'; // junction number is compulsory information
        }

        verified = verified && _.isEmpty(message);
        return message;
      };

      var verified = true;

      _.each(_.groupBy(current.node.junctions, 'junctionNumber'), function (junctions) {
        eventbus.trigger('junction:setCustomValidity', junctions, errorMessage(junctions));
      });

      return verified;
    };

    var validateJunctionPointAddress = function (idString, addr) {
      var message = '';

      if (addr < 1) {
        message = 'Tieosan keskellä olevan liittymäkohdan osoitteen on oltava vähintään yksi'; // User is not allowed to set the address smaller than one
      } else if (_.isNaN(addr)) {
        message = 'Liittymäkohdan osoite on pakollinen tieto'; // junction point address is a compulsory information
      }

      eventbus.trigger('junctionPoint:setCustomValidity', idString, message);
      return _.isEmpty(message);
    };

    var isDirty = function () {
      var original = false;
      if (current.node.nodeNumber) {
        original = nodeCollection.getNodeByNodeNumber(current.node.nodeNumber);
      }
      var nodePointsEquality = false;
      var junctionsEquality = false;
      var junctionPointsEquality = false;
      //  comparing nodes without junctions or nodePoints
      var nodesEquality = isEqualWithout(original, current.node, ['junctions', 'nodePoints']);
      //  comparing the nodePoints of both nodes
      if (original && original.nodePoints && original.nodePoints.length !== 0 && original.nodePoints.length === current.node.nodePoints.length) {
        nodePointsEquality = !_.some(_.flatMap(_.zip(_.sortBy(original.nodePoints, 'id'), _.sortBy(current.node.nodePoints, 'id')), _.spread(function (originalNodePoint, currentNodePoint) {
          return {equality: isEqualWithout(originalNodePoint, currentNodePoint, 'coordinates')};
        })), ['equality', false]);
      }
      //  comparing the junctions of both nodes
      if (original && original.junctions && original.junctions.length !== 0 && original.junctions.length === current.node.junctions.length) {
        junctionsEquality = !_.some(_.flatMap(_.zip(_.sortBy(original.junctions, 'id'), _.sortBy(current.node.junctions, 'id')), _.spread(function (originalJunction, currentJunction) {
          return {equality: isEqualWithout(originalJunction, currentJunction, 'junctionPoints')};
        })), ['equality', false]);

        //  comparing the junctionPoints of all junctions in both nodes
        junctionPointsEquality = !_.some(_.flatMap(_.zip(_.sortBy(original.junctions, 'id'), _.sortBy(current.node.junctions, 'id')), _.spread(function (originalJunction, currentJunction) {
          if (originalJunction.junctionPoints.length === currentJunction.junctionPoints.length && originalJunction.junctionPoints.length !== 0) {
            return _.flatMap(_.zip(originalJunction.junctionPoints, currentJunction.junctionPoints), _.spread(function (originalJunctionPoint, currentJunctionPoint) {
              return {equality: isEqualWithout(originalJunctionPoint, currentJunctionPoint, 'coordinates')};
            }));
          } else return false;
        })), ['equality', false]);
      }
      //  true equality implemented
      return !(nodesEquality && nodePointsEquality && junctionsEquality && junctionPointsEquality);
    };

    var isObsoleteNode = function () {
      return _.isEmpty(current.node.junctions) && _.isEmpty(_.filter(current.node.nodePoints, function (np) {
        return np.type !== LinkValues.NodePointType.CalculatedNodePoint.value;
      }));
    };

    var isEqualWithout = function (original, currentObject, toIgnore) {
      return _.isEqual(
        _.omit(original, toIgnore),
        _.omit(currentObject, toIgnore)
      );
    };

    var clean = function () {
      current = {};
    };

    var close = function (options, params, cancel) {
      eventbus.trigger(options, params, cancel);
      eventbus.trigger('nodesAndJunctions:open');
    };

    var closeForm = function () {
      eventbus.trigger('nodeLayer:closeForm', current); // all nodes and junctions forms should listen to this trigger
    };

    var closeNode = function (cancel) {
      close('node:unselected', current.node, cancel);
      clean();
      eventbus.trigger('nodeLayer:refreshView');
    };

    var closeTemplates = function () {
      clean();
      close('templates:unselected');
    };

    var saveNode = function () {
      eventbus.trigger('node:save', current.node);
    };

    var updateNodesAndJunctionsMarker = function (junction) {
      eventbus.trigger('change:node', current.node, junction);
    };

    eventbus.on('selectedNodesAndJunctions:openTemplates', function (templatesToOpen) {
      openTemplates(templatesToOpen);
    });

    return {
      openNode: openNode,
      openNodePointTemplate: openNodePointTemplate,
      openJunctionTemplate: openJunctionTemplate,
      getCurrentNode: getCurrentNode,
      getCurrentTemplates: getCurrentTemplates,
      getJunctions: getJunctions,
      getNodePoints: getNodePoints,
      addNodePointTemplates: addNodePointTemplates,
      addJunctionTemplates: addJunctionTemplates,
      getStartingCoordinates: getStartingCoordinates,
      setStartingCoordinates: setStartingCoordinates,
      setCoordinates: setCoordinates,
      setNodeName: setNodeName,
      setNodeType: setNodeType,
      typeHasChanged: typeHasChanged,
      getInitialStartDate: getInitialStartDate,
      setStartDate: setStartDate,
      setJunctionNumber: setJunctionNumber,
      setJunctionPointAddress: setJunctionPointAddress,
      detachJunctionAndNodePoints: detachJunctionAndNodePoints,
      attachJunctionAndNodePoints: attachJunctionAndNodePoints,
      validateJunctionNumbers: validateJunctionNumbers,
      validateJunctionPointAddress: validateJunctionPointAddress,
      isDirty: isDirty,
      isObsoleteNode: isObsoleteNode,
      closeNode: closeNode,
      closeTemplates: closeTemplates,
      closeForm: closeForm,
      saveNode: saveNode,
      updateNodesAndJunctionsMarker: updateNodesAndJunctionsMarker
    };
  };
}(this));
