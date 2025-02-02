/* eslint-disable prefer-named-capture-group */
(function (root) {
  root.URLRouter = function (map, backend, models) {
    var Router = Backbone.Router.extend({
      initialize: function () {

        this.route(/^(\d+)$/, function (layer) {
          applicationModel.selectLayer(layer);
        });

        this.route(/^([A-Za-z]+)\/?$/, function (layer) {
          if (layer === 'linkProperty') {
            applicationModel.selectLayer('linkProperty');
          } else {
            applicationModel.selectLayer(layer);
          }
        });

        this.route(/^$/, function () {
          applicationModel.selectLayer('linkProperty');
        });
      },

      routes: {
        'linkProperty/:linkId': 'linkProperty',
        'linkProperty/mml/:mmlId': 'linkPropertyByMml',
        'linkProperty/mtkid/:mtkid': 'linkPropertyByMtk',
        'roadAddressProject/:projectId': 'roadAddressProject',
        'historyLayer/:date': 'historyLayer',
        'node/nodePointTemplate/:id': 'nodePointTemplate',
        'node/junctionTemplate/:id': 'junctionTemplate'
      },

      linkProperty: function (linkId) {
        applicationModel.selectLayer('linkProperty');
        backend.getRoadAddressByLinkId(linkId, function (response) {
          if (response.success) {
            map.getView().setCenter([response.middlePoint.x, response.middlePoint.y]);
            map.getView().setZoom(zoomlevels.minZoomForLinkSearch);
          } else {
            console.log(response.reason);
          }
        });
      },

      linkPropertyByMml: function (mmlId) {
        applicationModel.selectLayer('linkProperty');
        backend.getRoadLinkByMmlId(mmlId, function (response) {
          eventbus.once('linkProperties:available', function () {
            models.selectedLinkProperty.open(response.id);
          });
          map.getView().setCenter([response.middlePoint.x, response.middlePoint.y]);
          map.getView().setZoom(12);
        });
      },

      linkPropertyByMtk: function (mtkid) {
        applicationModel.selectLayer('linkProperty');
        backend.getRoadLinkByMtkId(mtkid, function (response) {
          eventbus.once('linkProperties:available', function () {
            models.selectedLinkProperty.open(response.id);
          });
          map.getView().setCenter([response.x, response.y]);
          map.getView().setZoom(12);
        });
      },
      roadAddressProject: function (projectId) {
        applicationModel.selectLayer('roadAddressProject');
        eventbus.trigger('underConstructionProjectRoads:toggleVisibility', false);
        var parsedProjectId = parseInt(projectId);
        eventbus.trigger('roadAddressProject:startProject', parsedProjectId, true);
      },

      historyLayer: function (date) {
        applicationModel.selectLayer('linkProperty');
        var dateSeparated = date.split('-');
        eventbus.trigger('underConstructionProjectRoads:toggleVisibility', false);
        eventbus.trigger('underConstructionRoads:toggleVisibility', false);
        $('.underconstruction-visible-wrapper').hide();
        $('#toggleEditMode').hide();
        $('#emptyFormDiv,#formProjectButton').hide();
        eventbus.trigger('linkProperty:fetchHistoryLinks', dateSeparated);
      },

      nodePointTemplate: function (nodePointTemplateId) {
        eventbus.trigger('nodeSearchTool:clickNodePointTemplate', nodePointTemplateId);
      },

      junctionTemplate: function (junctionTemplateId) {
        eventbus.trigger('nodeSearchTool:clickJunctionTemplate', junctionTemplateId);
      }
    });


    var router = new Router();

    // We need to restart the router history so that tests can reset
    // the application before each test.
    Backbone.history.stop();
    Backbone.history.start();

    eventbus.on('linkProperties:unselected', function () {
      router.navigate('linkProperty');
    });

    eventbus.on('roadAddressProject:selected', function (id, _layerName, _selectedLayer) {
      router.navigate('roadAddressProject/' + id);
    });

    eventbus.on('linkProperties:selected', function (linkProperty) {
      if (!_.isEmpty(models.selectedLinkProperty.get())) {
        if (_.isArray(linkProperty)) {
          router.navigate('linkProperty/' + _.head(linkProperty).linkId);
        } else {
          router.navigate('linkProperty/' + linkProperty.linkId);
        }
      }
    });

    eventbus.on('linkProperties:selectedProject', function (linkId, project) {
      if (typeof project.id !== 'undefined') {
        var baseUrl = 'roadAddressProject/' + project.id;
        var linkIdUrl = linkId ? '/' + linkId : '';
        router.navigate(baseUrl + linkIdUrl);
        var initialCenter = map.getView().getCenter();
        if (!_.isUndefined(project.coordX) && project.coordX !== 0 && !_.isUndefined(project.coordY) && project.coordY !== 0 && !_.isUndefined(project.zoomLevel) && project.zoomLevel !== 0) {
          applicationModel.selectLayer('linkProperty', false);
          map.getView().setCenter([project.coordX, project.coordY]);
          map.getView().setZoom(project.zoomLevel);
        } else if (typeof linkId !== 'undefined') {
          applicationModel.selectLayer('linkProperty', false);
          backend.getProjectLinkByLinkId(linkId, function (response) {
            map.getView().setCenter([response.middlePoint.x, response.middlePoint.y]);
          });
        }
        var newCenter = map.getView().getCenter();
        if (initialCenter[0] === newCenter[0] && initialCenter[1] === newCenter[1]) {
          applicationModel.refreshMap(zoomlevels.getViewZoom(map), map.getLayers().getArray()[0].getExtent(), newCenter);
        }
      }
    });

    eventbus.on('nodePointTemplate:open', function (nodePointTemplateId) {
      router.navigate('nodePointTemplate/' + nodePointTemplateId);
    });

    eventbus.on('layer:selected', function (layer) {
      let layerAdjusted = layer;
      if (layer.indexOf('/') === -1) {
        layerAdjusted = layer.concat('/');
      }
      router.navigate(layerAdjusted);
    });
  };
}(this));
