(function (root) {
  root.LinkPropertyLayer = function (map, roadLayer, selectedLinkProperty, roadCollection, linkPropertiesModel, applicationModel) {
    Layer.call(this, map);
    var me = this;

    var indicatorVector = new ol.source.Vector({});
    var anomalousMarkerVector = new ol.source.Vector({});
    var underConstructionMarkerVector = new ol.source.Vector({});
    var directionMarkerVector = new ol.source.Vector({});
    var calibrationPointVector = new ol.source.Vector({});
    var greenRoadLayerVector = new ol.source.Vector({});
    var pickRoadsLayerVector = new ol.source.Vector({});
    var simulationVector = new ol.source.Vector({});
    var geometryChangedVector = new ol.source.Vector({});
    var underConstructionRoadLayerVector = new ol.source.Vector({});
    var unAddressedRoadLayerVector = new ol.source.Vector({});
    var reservedRoadVector = new ol.source.Vector({});
    var historicRoadsVector = new ol.source.Vector({});

    var SelectionType = LinkValues.SelectionType;
    var Anomaly = LinkValues.Anomaly;
    var ConstructionType = LinkValues.ConstructionType;
    var SideCode = LinkValues.SideCode;
    var RoadZIndex = LinkValues.RoadZIndex;

    var isActiveLayer = false;
    var cachedMarker = null;

    var roadLinkStyler = new RoadLinkStyler();

    var indicatorLayer = new ol.layer.Vector({
      source: indicatorVector,
      name: 'indicatorLayer',
      zIndex: RoadZIndex.IndicatorLayer.value
    });
    indicatorLayer.set('name', 'indicatorLayer');

    var anomalousMarkerLayer = new ol.layer.Vector({
      source: anomalousMarkerVector,
      name: 'anomalousMarkerLayer',
      zIndex: RoadZIndex.IndicatorLayer.value
    });
    anomalousMarkerLayer.set('name', 'anomalousMarkerLayer');

    var directionMarkerLayer = new ol.layer.Vector({
      source: directionMarkerVector,
      name: 'directionMarkerLayer',
      zIndex: RoadZIndex.DirectionMarkerLayer.value
    });
    directionMarkerLayer.set('name', 'directionMarkerLayer');

    var underConstructionMarkerLayer = new ol.layer.Vector({
      source: underConstructionMarkerVector,
      name: 'underConstructionMarkerLayer',
      zIndex: RoadZIndex.DirectionMarkerLayer.value
    });
    underConstructionMarkerLayer.set('name', 'underConstructionMarkerLayer');

    var geometryChangedLayer = new ol.layer.Vector({
      source: geometryChangedVector,
      name: 'geometryChangedLayer',
      style: function (feature) {
        return [roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      },
      zIndex: RoadZIndex.GeometryChangedLayer.value
    });
    geometryChangedLayer.set('name', 'geometryChangedLayer');

    var calibrationPointLayer = new ol.layer.Vector({
      source: calibrationPointVector,
      name: 'calibrationPointLayer',
      zIndex: RoadZIndex.CalibrationPointLayer.value
    });
    calibrationPointLayer.set('name', 'calibrationPointLayer');

    var greenRoadLayer = new ol.layer.Vector({
      source: greenRoadLayerVector,
      name: 'greenRoadLayer',
      zIndex: RoadZIndex.GreenLayer.value
    });
    greenRoadLayer.set('name', 'greenRoadLayer');

    var reservedRoadLayer = new ol.layer.Vector({
      source: reservedRoadVector,
      name: 'reservedRoadLayer',
      zIndex: RoadZIndex.ReservedRoadLayer.value
    });
    reservedRoadLayer.set('name', 'reservedRoadLayer');

    var pickRoadsLayer = new ol.layer.Vector({
      source: pickRoadsLayerVector,
      name: 'pickRoadsLayer',
      style: function (feature) {
        return [roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });
    pickRoadsLayer.set('name', 'pickRoadsLayer');

    var simulatedRoadsLayer = new ol.layer.Vector({
      source: simulationVector,
      name: 'simulatedRoadsLayer',
      style: function (feature) {
        return [roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });
    simulatedRoadsLayer.set('name', 'simulatedRoadsLayer');


    var underConstructionRoadLayer = new ol.layer.Vector({
      source: underConstructionRoadLayerVector,
      name: 'underConstructionRoadLayer',
      style: function (feature) {
        return [roadLinkStyler.getBorderStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}), roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });
    underConstructionRoadLayer.set('name', 'underConstructionRoadLayer');

    var unAddressedRoadLayer = new ol.layer.Vector({
      source: unAddressedRoadLayerVector,
      name: 'unAddressedRoadLayer',
      style: function (feature) {
        return [roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });
    unAddressedRoadLayer.set('name', 'unAddressedRoadLayer');


    var historicRoadsLayer = new ol.layer.Vector({
      source: historicRoadsVector,
      name: 'historicRoadsLayer',
      style: function (feature) {
        return [roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      },
      zIndex: RoadZIndex.HistoricRoadLayer.value
    });
    historicRoadsLayer.set('name', 'historicRoadsLayer');


    var layers = [roadLayer.layer, anomalousMarkerLayer, directionMarkerLayer, underConstructionMarkerLayer, geometryChangedLayer, calibrationPointLayer,
      indicatorLayer, greenRoadLayer, pickRoadsLayer, simulatedRoadsLayer, underConstructionRoadLayer, unAddressedRoadLayer, reservedRoadLayer, historicRoadsLayer];

    var setGeneralOpacity = function (opacity) {
      roadLayer.layer.setOpacity(opacity);
      anomalousMarkerLayer.setOpacity(opacity);
      directionMarkerLayer.setOpacity(opacity);
      underConstructionMarkerLayer.setOpacity(opacity);
      underConstructionRoadLayer.setOpacity(opacity);
      unAddressedRoadLayer.setOpacity(opacity);
      historicRoadsLayer.setOpacity(opacity);
      geometryChangedLayer.setOpacity(opacity);
    };

    /**
     * We declare the type of interaction we want the map to be able to respond.
     * A selected feature is moved to a new/temporary layer out of the default roadLayer.
     * This interaction is restricted to a double click.
     * @type {ol.interaction.Select}
     *
     *
     */
    var selectDoubleClick = new ol.interaction.Select({
      //Multi is the one en charge of defining if we select just the feature we clicked or all the overlapping
      //multi: true,
      //This will limit the interaction to the specific layer, in this case the layer where the roadAddressLinks are drawn
      layer: [roadLayer.layer, geometryChangedLayer, underConstructionRoadLayer, unAddressedRoadLayer, historicRoadsLayer],
      //Limit this interaction to the doubleClick
      condition: ol.events.condition.doubleClick,
      //The new/temporary layer needs to have a style function as well, we define it here.
      style: function (feature) {
        return [roadLinkStyler.getBorderStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });

    var getSelectedF = (ctrlPressed, event) => {
      // if ctrl is pressed, we return the raw selection so that we get the linkData we can add to the selection
      if (ctrlPressed) {
        return map.forEachFeatureAtPixel(event.mapBrowserEvent.pixel, function (feature) {
          return feature;
        });
      } else {
        // if not, then we want the selection to be undefined if we click a link that was already clicked
        // OR  if the link that is not already selected was clicked, we get linkData
        return _.find(event.selected, function (selectionTarget) {
          return !_.isUndefined(selectionTarget.linkData);
        });
      }
    };

    /**
     * We now declare what kind of custom actions we want when the interaction happens.
     * Note that 'select' is triggered when a feature is either selected or deselected.
     * The event holds the selected features in the events.selected and the deselected in event.deselected.
     */
    selectDoubleClick.on('select', function (event) {
      var visibleFeatures = getVisibleFeatures(true, true, true, false, false, true, true, true, true);
      selectSingleClick.getFeatures().clear();
      var ctrlPressed = (event.mapBrowserEvent) ? event.mapBrowserEvent.originalEvent.ctrlKey : false;

      if (applicationModel.isReadOnly()) {
        selectDoubleClick.getFeatures().clear();
      }
      //Since the selected features are moved to a new/temporary layer we just need to reduce the roadlayer's opacity levels.
      if (event.selected.length !== 0) {
        var selectedF = getSelectedF(ctrlPressed, event);
        if (roadLayer.layer.getOpacity() === 1) {
          setGeneralOpacity(0.2);
        }
        if (!_.isUndefined(selectedF)) {
          var selection = selectedF.linkData;
          if (ctrlPressed) { // if ctrl button was pressed while double clicking the link then we want to add the selected link to the selection
            modifyPreviousSelection(ctrlPressed, selection);
          } else { // otherwise we want to select just the double clicked link
            selectedLinkProperty.open(selection, false, visibleFeatures);
          }
        }
      }
    });
    selectDoubleClick.set('name', 'selectDoubleClickInteractionLPL');


    var zoomDoubleClickListener = function (_event) {
      if (isActiveLayer)
        _.defer(function () {
          if (selectedLinkProperty.get().length === 0 && zoomlevels.getViewZoom(map) <= 13) {
            map.getView().setZoom(Math.trunc(map.getView().getZoom() + 1));
          }
        });
    };
    //This will control the double click zoom when there is no selection that activates
    map.on('dblclick', zoomDoubleClickListener);

    /**
     * We declare the type of interaction we want the map to be able to respond.
     * A selected feature is moved to a new/temporary layer out of the default roadLayer.
     * This interaction is restricted to a single click (there is a 250 ms enforced
     * delay between single clicks in order to differentiate from double click).
     * @type {ol.interaction.Select}
     */
    var selectSingleClick = new ol.interaction.Select({
      //Multi is the one en charge of defining if we select just the feature we clicked or all the overlapping
      multi: true,
      //This will limit the interaction to the specific layer, in this case the layer where the roadAddressLinks are drawn
      layers: [roadLayer.layer, anomalousMarkerLayer, greenRoadLayer, pickRoadsLayer, geometryChangedLayer,
        underConstructionRoadLayer, unAddressedRoadLayer, historicRoadsLayer],
      //Limit this interaction to the singleClick
      condition: ol.events.condition.singleClick,
      filter: function (feature) {
        var currentSelectionType = applicationModel.getSelectionType().value;
        if (currentSelectionType === SelectionType.Unknown.value) {
          return feature.linkData.anomaly !== Anomaly.None.value && feature.linkData.roadLinkType === RoadLinkType.UnknownRoadLinkType.value;
        } else {
          return currentSelectionType === SelectionType.All.value;
        }
      },
      style: function (feature) {
        return [roadLinkStyler.getBorderStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}),
          roadLinkStyler.getOverlayStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)})];
      }
    });
    selectSingleClick.set('name', 'selectSingleClickInteractionLPL');


    /**
     * We now declare what kind of custom actions we want when the interaction happens.
     * Note that 'select' is triggered when a feature is either selected or deselected.
     * The event holds the selected features in the events.selected and the deselected in event.deselected.
     *
     * In this particular case we are fetching every roadLinkAddress and anomaly marker in view and
     * sending them to the selectedLinkProperty.open for further processing,
     * or adding them to the selection if user pressed ctrl button while clicking.
     */
    selectSingleClick.on('select', function (event) {
      var ctrlPressed = (event.mapBrowserEvent) ? event.mapBrowserEvent.originalEvent.ctrlKey : false;
      var visibleFeatures = getVisibleFeatures(true, true, true, true, true, true, true, true, true);
      selectDoubleClick.getFeatures().clear();

      var selectedF = getSelectedF(ctrlPressed, event);

      if (selectedF) {
        var selection = selectedF.linkData;
        if (roadLayer.layer.getOpacity() === 1) {
          setGeneralOpacity(0.2);
        }
        if (ctrlPressed) {  // if ctrl button was pressed while single clicking then we want to add the clicked link to the previous selection
          modifyPreviousSelection(ctrlPressed, selection);
        } else { // otherwise we want to select the whole road part
          selectedLinkProperty.close();
          setGeneralOpacity(0.2);
          if (selection.roadNumber !== 0) {
            applicationModel.addSpinner();
            // set the clicked linear location id so we know what road link group to update after fetching road links in backend
            roadCollection.setClickedLinearLocationId(selection.linearLocationId);
            // gets all the road links from backend and starts a cycle that updates road link group in RoadCollection.js
            roadCollection.fetchWholeRoadPart(selection.roadNumber, selection.roadPartNumber);

            // listens to the event when the road link group is updated (with whole roadpart) and then continues the process normally with the updated road link groups
            eventbus.listenTo(eventbus,'roadCollection:wholeRoadPartFetched', function () {
              applicationModel.removeSpinner();
              var features = getAllFeatures();
              selectedLinkProperty.open(selection, true, features);
            });
          }
          // opens only the visible parts of the roads (bounding box view)
          selectedLinkProperty.open(selection, true, visibleFeatures);
        }
      } else { // if selectedF was undefined we want to deselect all selected links
        selectedLinkProperty.close();
      }
    });

    map.on('click', function (event) {
      //The addition of the check for features on point and the selection mode
      // seem to fix the problem with the clicking on the empty map after being in the defloating process would allow a deselection and enabling of the menus
      if (window.getSelection) {
        window.getSelection().removeAllRanges();
      } //removes selection from forms
      else if (document.selection) {
        document.selection.empty();
      }
      var hasFeatureOnPoint = _.isUndefined(map.forEachFeatureAtPixel(event.pixel, function (feature) {
        return feature;
      }));
      var nonSpecialSelectionType = !_.includes(applicationModel.specialSelectionTypes, applicationModel.getSelectionType().value);
      if (isActiveLayer) {
        if (hasFeatureOnPoint && nonSpecialSelectionType) {
          selectedLinkProperty.close();
        }
      }
    });

    /**
     * Simple method that will add various open layers 3 features to a selection.
     * @param features
     */
    var addFeaturesToSelection = function (features) {
      var olUids = _.map(selectSingleClick.getFeatures().getArray(), function (feature) {
        return feature.ol_uid;
      });
      _.each(features, function (feature) {
        if (!_.includes(olUids, feature.ol_uid)) {
          selectSingleClick.getFeatures().push(feature);
          olUids.push(feature.ol_uid); // prevent adding duplicate entries
        }
      });
    };

    /**
     * Add/remove ctrl clicked link's:
     * linearLocationId to/from a list of previously selected linearLocationIds, if the clicked link has an address
     * OR
     * linkId to/from a list of previously selected linkIds, if the clicked link is unaddressed
     *
     * There are 2 reasons why selections are divided into linkIds and linearLocationIds:
     * 1) a link with an address might have a "shared" linkId with another link
     *    and clicking one of those links would select/deselect both of those links.
     *
     * 2) unaddressed links all have linearLocationId set to 0 (zero).
     *    So only using linearLocationId would select/deselect all of them
     *
     * So to counter those points:
     * - Links with an address are kept track of with a list of linearLocationIds,
     * - Unaddressed links are kept track of with a list of linkIds.
     *
     * These two modified lists are then passed on to a function called openCtrl
     * @param ctrlPressed - boolean
     * @param selection - link data of the clicked link
     *
     * */
    var modifyPreviousSelection = function (ctrlPressed, selection) {
      var modifiedList = function (listOfIds, id) {
        if (_.includes(listOfIds, id)) {
          return _.without(listOfIds, id);
        } else {
          return listOfIds.concat(id);
        }
      };
      if (ctrlPressed && !_.isUndefined(selectedLinkProperty.get()) && !_.isUndefined(selection)) {

        var [selectedWithAddress, selectedUnaddressed] = _.partition(selectedLinkProperty.get(), function (selected) {
          return selected.linearLocationId !== 0;
        });

        var selectedLinearLocationIds = _.map(selectedWithAddress, function (selected) {
          return selected.linearLocationId;
        });

        var selectedLinkIds = _.map(selectedUnaddressed, function (selected) {
          return selected.linkId;
        });

        if (selection.linearLocationId === 0) {
          selectedLinkIds = modifiedList(selectedLinkIds, selection.linkId);
        } else {
          selectedLinearLocationIds = modifiedList(selectedLinearLocationIds, selection.linearLocationId);
        }

        if (selectedLinearLocationIds.length === 0 && selectedLinkIds.length === 0) {
          // if both lists are empty then the last selected link was "deselected" and we want the UI to behave like no links are currently selected
          selectedLinkProperty.close();
        } else {
          var features = getAllFeatures();
          // pass the lists to further processing
          selectedLinkProperty.openCtrl(selectedLinearLocationIds, selectedLinkIds, true, features);
        }
      }
    };

    /**
     * Event triggered by the selectedLinkProperty.open() returning all the open layers 3 features
     * that need to be included in the selection.
     */
    me.eventListener.listenTo(eventbus, 'linkProperties:olSelected', function (features) {
      clearHighlights();
      addFeaturesToSelection(features);
    });

    var getVisibleFeatures = function (withRoads, withAnomalyMarkers, withGreenRoads, withPickRoads, withDirectionalMarkers, withunderConstructionRoads, withGeometryChanged, withVisibleUnAddressedRoads) {
      var extent = map.getView().calculateExtent(map.getSize());
      var visibleRoads = withRoads ? roadLayer.layer.getSource().getFeaturesInExtent(extent) : [];
      var visibleAnomalyMarkers = withAnomalyMarkers ? anomalousMarkerLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleGreenRoadLayer = withGreenRoads ? greenRoadLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleDirectionalMarkers = withDirectionalMarkers ? directionMarkerLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleUnderConstructionMarkers = withDirectionalMarkers ? underConstructionMarkerLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleUnderConstructionRoads = withunderConstructionRoads ? underConstructionRoadLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleUnAddressedRoads = withVisibleUnAddressedRoads ? unAddressedRoadLayer.getSource().getFeaturesInExtent(extent) : [];
      var visibleGeometryChanged = withGeometryChanged ? geometryChangedLayer.getSource().getFeaturesInExtent(extent) : [];
      return visibleRoads.concat(visibleAnomalyMarkers).concat(visibleGreenRoadLayer).concat(visibleDirectionalMarkers).concat(visibleUnderConstructionMarkers).concat(visibleUnderConstructionRoads).concat(visibleUnAddressedRoads).concat(visibleGeometryChanged);
    };

    var getAllFeatures = function () {
      var roads = roadLayer.layer.getSource().getFeatures();
      var anomalyMarkers = anomalousMarkerLayer.getSource().getFeatures();
      var greenRoadLayerFeatures = greenRoadLayer.getSource().getFeatures();
      var directionalMarkers = directionMarkerLayer.getSource().getFeatures();
      var underConstructionMarkers = underConstructionMarkerLayer.getSource().getFeatures();
      var underConstructionRoads = underConstructionRoadLayer.getSource().getFeatures();
      var unAddressedRoads = unAddressedRoadLayer.getSource().getFeatures();
      var geometryChanged = geometryChangedLayer.getSource().getFeatures();
      return roads.concat(anomalyMarkers).concat(greenRoadLayerFeatures).concat(directionalMarkers).concat(underConstructionMarkers).concat(underConstructionRoads).concat(unAddressedRoads).concat(geometryChanged);
    };

    /**
     * This will add all the following interactions from the map:
     * -selectDoubleClick
     * -selectSingleClick
     */
    var addSelectInteractions = function () {
      map.addInteraction(selectDoubleClick);
      map.addInteraction(selectSingleClick);
    };

    /**
     * This will remove all the following interactions from the map:
     * -selectDoubleClick
     * -selectSingleClick
     */
    var removeSelectInteractions = function () {
      map.removeInteraction(selectDoubleClick);
      map.removeInteraction(selectSingleClick);
    };

    //We add the defined interactions to the map.
    addSelectInteractions();

    var unselectRoadLink = function () {
      indicatorLayer.getSource().clear();
      greenRoadLayer.getSource().clear();
      _.map(roadLayer.layer.getSource().getFeatures(), function (feature) {
        if (feature.linkData.gapTransfering) {
          feature.linkData.gapTransfering = false;
          feature.linkData.anomaly = feature.linkData.prevAnomaly;
          var unknownRoadStyle = roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)});
          feature.setStyle(unknownRoadStyle);
        }
      });
    };

    var redraw = function () {
      cachedMarker = new LinkPropertyMarker(selectedLinkProperty);
      removeSelectInteractions();
      var allRoadLinks = roadCollection.getAll();
      var underConstructionLinks = roadCollection.getUnderConstructionLinks();
      var roadLinks = _.reject(allRoadLinks, function (rl) {
        return _.includes(_.map(underConstructionLinks, function (sl) {
          return sl.linkId;
        }), rl.linkId);
      });
      me.clearLayers([anomalousMarkerLayer, geometryChangedLayer, underConstructionRoadLayer, unAddressedRoadLayer, directionMarkerLayer, underConstructionMarkerLayer, calibrationPointLayer]);

      if (zoomlevels.getViewZoom(map) >= zoomlevels.minZoomForRoadNetwork) {

        var directionRoadMarker = _.filter(roadLinks, function (roadlink) {
          return roadlink.anomaly !== Anomaly.NoAddressGiven.value && roadlink.anomaly !== Anomaly.GeometryChanged.value && (roadlink.sideCode === SideCode.AgainstDigitizing.value || roadlink.sideCode === SideCode.TowardsDigitizing.value);
        });

        if (zoomlevels.getViewZoom(map) > zoomlevels.minZoomForDirectionalMarkers) {
          _.each(directionRoadMarker, function (directionLink) {
            cachedMarker.createMarker(directionLink, function (marker) {
              directionMarkerLayer.getSource().addFeature(marker);
            });
          });

          _.each(underConstructionLinks, function (directionLink) {
            cachedMarker.createMarker(directionLink, function (marker) {
              underConstructionMarkerLayer.getSource().addFeature(marker);
            });
          });
        }

        var anomalousRoadMarkers = _.filter(roadLinks, function (roadlink) {
          return roadlink.anomaly !== Anomaly.None.value;
        });
        _.each(anomalousRoadMarkers, function (anomalouslink) {
          cachedMarker.createMarker(anomalouslink, function (marker) {
            anomalousMarkerLayer.getSource().addFeature(marker);
          });
        });

        var geometryChangedRoadMarkers = _.filter(roadLinks, function (roadlink) {
          return roadlink.anomaly === Anomaly.GeometryChanged.value;
        });

        _.each(geometryChangedRoadMarkers, function (geometryChangedLink) {

          var newLinkData = Object.assign({}, geometryChangedLink);
          newLinkData.roadClass = 99;
          newLinkData.roadLinkSource = 99;
          newLinkData.sideCode = 99;
          newLinkData.linkType = 99;
          newLinkData.constructionType = 0;
          newLinkData.roadLinkType = 0;
          newLinkData.id = 0;
          newLinkData.startAddressM = "";
          newLinkData.endAddressM = "";
          newLinkData.anomaly = Anomaly.NoAddressGiven.value;
          newLinkData.points = newLinkData.newGeometry;

          cachedMarker.createMarker(newLinkData, function (marker) {
            geometryChangedLayer.getSource().addFeature(marker);
          });

          var points = _.map(newLinkData.newGeometry, function (point) {
            return [point.x, point.y];
          });
          var feature = new ol.Feature({geometry: new ol.geom.LineString(points)});
          feature.linkData = newLinkData;
          roadCollection.addTmpRoadLinkGroups(newLinkData);
          geometryChangedLayer.getSource().addFeature(feature);
        });

        //Removed the need to check if the buttons are active in order to draw calibration points.
        if (zoomlevels.getViewZoom(map) >= zoomlevels.minZoomLevelForCalibrationPoints) {
          var actualPoints = me.drawCalibrationMarkers(calibrationPointLayer.source, roadLinks);
          _.each(actualPoints, function (actualPoint) {
            var calMarker = new CalibrationPoint(actualPoint);
            calibrationPointLayer.getSource().addFeature(calMarker.getMarker(true));
          });
        }
      }
      addSelectInteractions();
      if (applicationModel.getCurrentAction() === -1) {
        applicationModel.removeSpinner();
      }
    };

    this.refreshView = function () {
      //Generalize the zoom levels as the resolutions and zoom levels differ between map tile sources
      roadCollection.reset();
      roadCollection.fetch(map.getView().calculateExtent(map.getSize()), zoomlevels.getViewZoom(map));
      underConstructionRoadLayer.changed();
      unAddressedRoadLayer.changed();
      roadLayer.layer.changed();
    };

    this.isDirty = function () {
      return selectedLinkProperty.isDirty();
    };

    var handleLinkPropertyChanged = function (eventListener) {
      removeSelectInteractions();
      eventListener.stopListening(eventbus, 'map:clicked', me.displayConfirmMessage);
      eventListener.listenTo(eventbus, 'map:clicked', me.displayConfirmMessage);
    };

    var concludeLinkPropertyEdit = function (eventListener) {
      addSelectInteractions();
      eventListener.stopListening(eventbus, 'map:clicked', me.displayConfirmMessage);
      geometryChangedLayer.setVisible(false);
      setGeneralOpacity(1);
      if (selectDoubleClick.getFeatures().getLength() !== 0) {
        selectDoubleClick.getFeatures().clear();
      }
    };

    this.layerStarted = function (eventListener) {
      var linkPropertyChangeHandler = _.partial(handleLinkPropertyChanged, eventListener);
      var linkPropertyEditConclusion = _.partial(concludeLinkPropertyEdit, eventListener);
      eventListener.listenTo(eventbus, 'linkProperties:changed', linkPropertyChangeHandler);
      eventListener.listenTo(eventbus, 'linkProperties:cancelled linkProperties:saved', linkPropertyEditConclusion);

      eventListener.listenTo(eventbus, 'linkProperties:selected linkProperties:multiSelected', function (link) {
        let selectedLink = link;
        if (link) {
          selectedLink = (_.isArray(link)) ? link : [link];
        }
        var isUnknown = _.every(selectedLink, function (sl) {
          return sl.anomaly !== Anomaly.None.value;
        });
        var roads = isUnknown ? geometryChangedLayer.getSource().getFeatures() : roadLayer.layer.getSource().getFeatures();
        var features = [];
        _.each(selectedLink, function (featureLink) {
          if (selectedLinkProperty.canOpenByLinearLocationId(featureLink.linearLocationId)) {
            _.each(roads, function (feature) {
              if (_.includes(featureLink.selectedLinks, feature.linkData.linearLocationId))
                return features.push(feature);
              return features;
            });
          } else if (featureLink.linkId !== 0) {
            _.each(roads, function (feature) {
              if (_.includes(featureLink.selectedLinks, feature.linkData.linkId))
                return features.push(feature);
              return features;
            });
          }
        });
        if (features) {
          addFeaturesToSelection(features);
        }
        clearIndicators();
      });

      eventListener.listenTo(eventbus, 'roadLinks:fetched', function () {
        if (applicationModel.getSelectedLayer() === 'linkProperty') {
          redraw();
        }
      });
      eventListener.listenTo(eventbus, 'underConstructionRoadLinks:fetched', function (underConstructionRoads) {
        var partitioned = _.partition(_.flatten(underConstructionRoads), function (feature) {
          return feature.getData().constructionType === ConstructionType.UnderConstruction.value && feature.getData().roadNumber === 0;
        });
        var ol3underConstructionRoads =
          _.map(partitioned[0], function (road) {
            var roadData = road.getData();
            var points = _.map(roadData.points, function (point) {
              return [point.x, point.y];
            });
            var feature = new ol.Feature({
              geometry: new ol.geom.LineString(points)
            });
            feature.linkData = roadData;
            return feature;
          });
        underConstructionRoadLayer.getSource().clear();
        underConstructionRoadLayer.getSource().addFeatures(ol3underConstructionRoads);

        var ol3noInfoRoads =
          _.map(partitioned[1], function (road) {
            var roadData = road.getData();
            var points = _.map(roadData.points, function (point) {
              return [point.x, point.y];
            });
            var feature = new ol.Feature({
              geometry: new ol.geom.LineString(points)
            });
            feature.linkData = roadData;
            return feature;
          });
        roadLayer.layer.getSource().addFeatures(ol3noInfoRoads);
      });

      eventListener.listenTo(eventbus, 'unAddressedRoadLinks:fetched', function (unAddressedRoads) {

        var ol3noInfoRoads =
          _.map(_.flatten(unAddressedRoads), function (road) {
            var roadData = road.getData();
            var points = _.map(roadData.points, function (point) {
              return [point.x, point.y];
            });
            var feature = new ol.Feature({
              geometry: new ol.geom.LineString(points)
            });
            feature.linkData = roadData;
            return feature;
          });
        unAddressedRoadLayer.getSource().clear();
        unAddressedRoadLayer.getSource().addFeatures(ol3noInfoRoads);
      });
      eventListener.listenTo(eventbus, 'unAddressedRoads:toggleVisibility', function (visibility) {
        unAddressedRoadLayer.setVisible(visibility);
      });
      eventListener.listenTo(eventbus, 'underConstructionRoads:toggleVisibility', function (visibility) {
        underConstructionRoadLayer.setVisible(visibility);
        underConstructionMarkerLayer.setVisible(visibility);
      });
      eventListener.listenTo(eventbus, 'linkProperty:visibilityChanged', function () {
        //Exclude underConstruction layers from toggle
        me.toggleLayersVisibility([roadLayer.layer, anomalousMarkerLayer, directionMarkerLayer, geometryChangedLayer, calibrationPointLayer,
          indicatorLayer, greenRoadLayer, pickRoadsLayer, simulatedRoadsLayer, reservedRoadLayer, historicRoadsLayer], applicationModel.getRoadVisibility());
      });
      eventListener.listenTo(eventbus, 'linkProperties:dataset:changed', redraw);
      eventListener.listenTo(eventbus, 'linkProperties:updateFailed', cancelSelection);

      eventListener.listenTo(eventbus, 'linkProperties:clearIndicators', function () {
        clearIndicators();
      });

      eventListener.listenTo(eventbus, 'roadLinks:refreshView', function () {
        me.refreshView();
      });

      var clearIndicators = function () {
        indicatorLayer.getSource().clear();
      };

      eventListener.listenTo(eventListener, 'map:clearLayers', me.clearLayers);
    };

    var cancelSelection = function () {
      if (!applicationModel.isActiveButtons()) {
        selectedLinkProperty.cancel();
        selectedLinkProperty.close();
        unselectRoadLink();
      }
    };


    me.eventListener.listenTo(eventbus, 'linkProperties:highlightSelectedProject', function (featureLinkId) {
      setGeneralOpacity(0.2);
      var boundingBox = map.getView().calculateExtent(map.getSize());
      var zoomLevel = zoomlevels.getViewZoom(map);
      roadCollection.findReservedProjectLinks(boundingBox, zoomLevel, featureLinkId);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:highlightReservedRoads', function (reservedOLFeatures) {
      var styledFeatures = _.map(reservedOLFeatures, function (feature) {
        feature.setStyle(roadLinkStyler.getRoadLinkStyle().getStyle(feature.linkData, {zoomLevel: zoomlevels.getViewZoom(map)}));
        return feature;
      });
      if (applicationModel.getSelectedLayer() === "linkProperty") { //check if user is still in reservation form
        reservedRoadLayer.getSource().addFeatures(styledFeatures);
      }
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:deselectFeaturesSelected', function () {
      clearHighlights();
      geometryChangedLayer.setVisible(true);
    });

    me.eventListener.listenTo(eventbus, 'linkProperty:fetch', function () {
      map.getView().setZoom(Math.round(zoomlevels.getViewZoom(map)));
      roadCollection.fetch(map.getView().calculateExtent(map.getSize()).join(','), zoomlevels.getViewZoom(map) + 1);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:activateInteractions', function () {
      toggleSelectInteractions(true, true);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:deactivateInteractions', function () {
      toggleSelectInteractions(false, true);
    });

    me.eventListener.listenTo(eventbus, 'linkProperty:fetchedHistoryLinks', function (historyLinkData) {
      var points = _.map(historyLinkData.geometry, function (point) {
        return [point.x, point.y];
      });
      var historyFeatures = _.map(historyLinkData, function (link) {
        var feature = new ol.Feature({
          geometry: new ol.geom.LineString(points)
        });
        feature.linkData = link;
        return feature;
      });
      historicRoadsLayer.getSource().addFeatures(historyFeatures);
    });

    me.eventListener.listenTo(eventbus, 'linkProperty:fetchHistoryLinks', function (date) {
      roadCollection.setDate(date);
      roadCollection.fetch(map.getView().calculateExtent(map.getSize()), zoomlevels.getViewZoom(map));
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:unselected', function () {
      clearHighlights();
      setGeneralOpacity(1);
      if (greenRoadLayer.getSource().getFeatures().length !== 0) {
        unselectRoadLink();
      }
      if (indicatorLayer.getSource().getFeatures().length !== 0) {
        indicatorLayer.getSource().clear();
      }
      if (applicationModel.selectionTypeIs(SelectionType.Unknown)) {
        setGeneralOpacity(0.2);
        anomalousMarkerLayer.setOpacity(1);
      }
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:deactivateDoubleClick', function () {
      toggleSelectInteractions(false, false);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:deactivateAllSelections roadAddressProject:deactivateAllSelections', function () {
      toggleSelectInteractions(false, true);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:activateDoubleClick', function () {
      toggleSelectInteractions(true, false);
    });

    me.eventListener.listenTo(eventbus, 'linkProperties:activateAllSelections roadAddressProject:startAllInteractions', function () {
      toggleSelectInteractions(true, true);
    });

    me.eventListener.listenTo(eventbus, 'layer:selected', function (layer, previouslySelectedLayer) {
      isActiveLayer = layer === 'linkProperty';
      toggleSelectInteractions(isActiveLayer, true);
      if (isActiveLayer) {
        addSelectInteractions();
      } else {
        removeSelectInteractions();
      }
      me.clearLayers(layers);
      clearHighlights();
      if (previouslySelectedLayer === 'linkProperty') {
        hideLayer();
        removeSelectInteractions();
      } else {
        setGeneralOpacity(1);
        showLayer();
        eventbus.trigger('linkProperty:fetch');
      }
      me.toggleLayersVisibility(layers, applicationModel.getRoadVisibility());
    });

    var clearHighlights = function () {
      selectSingleClick.getFeatures().clear();
      selectDoubleClick.getFeatures().clear();
      map.updateSize();
    };

    var toggleSelectInteractions = function (activate, both) {
      selectDoubleClick.setActive(activate);
      if (both) {
        selectSingleClick.setActive(activate);
      }
    };

    me.eventListener.listenTo(eventbus, 'roadAddressProject:clearOnClose', function () {
      setGeneralOpacity(1);
      reservedRoadLayer.getSource().clear();
      applicationModel.setReadOnly(true);
    });

    var showLayer = function () {
      me.start();
      me.layerStarted(me.eventListener);
    };

    var hideLayer = function () {
      unselectRoadLink();
      me.clearLayers(layers);
    };

    me.toggleLayersVisibility(layers, true);
    me.addLayers(layers);
    me.layerStarted(me.eventListener);

    return {
      show: showLayer,
      hide: hideLayer,
      minZoomForContent: me.minZoomForContent
    };
  };
}(this));
