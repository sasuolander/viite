(function (root) {
  root.ProjectListModel = function (projectCollection) {
    var projectStatus = LinkValues.ProjectStatus;
    var statusToDisplay = LinkValues.ProjectStatusToDisplay;
    var projectArray = [];
    var headers = {
      "sortName": {
        toStr: "PROJEKTIN NIMI", width: "255",
        sortFunc: function (a, b) {
          return a.name.localeCompare(b.name, 'fi');
        }
      },
      "sortELY": {
        toStr: "ELY", width: "50",
        sortFunc: function (a, b) {
          return a.ely - b.ely;
        }
      },
      "sortUser": {
        toStr: "KÄYTTÄJÄ", width: "115",
        sortFunc: function (a, b) {
          return a.createdBy.localeCompare(b.createdBy, 'fi');
        }
      },
      "sortDate": {
        toStr: "LUONTIPVM", width: "110",
        sortFunc: function (a, b) {
          var aDate = a.createdDate.split('.').reverse().join('-');
          var bDate = b.createdDate.split('.').reverse().join('-');
          return new Date(bDate) - new Date(aDate);
        }
      },
      "sortStatus": {
        toStr: "TILA", width: "60",
        sortFunc: function (a, b) {
          return a.statusCode - b.statusCode;
        }
      }
    };

    var orderBy = {
      id: "sortStatus", reversed: false
    };

    var filterBox = {
      input: "", visible: false
    };

    var getIcon = function (id) {
      if (orderBy.id === id) {
        if (orderBy.reversed) {
          return 'fa-sort-down';
        } else {
          return 'fa-sort-up';
        }
      } else {
        return 'fa-sort';
      }
    };

    var headersToHtml = function () {
      var html = "";
      _.forEach(Object.keys(headers), function(id) {
          var header = headers[id];
          html += '<label class="content-new label" style="width: ' + header.width + 'px">' + header.toStr + '<i id=' + id + ' class="btn-icon sort fas ' + getIcon(id) + '"></i>';
          if (id === "sortUser") {
            html += '<i id="filterUser" class="btn-icon fas fa-filter"></i></label>' +
              '<span class="smallPopupContainer" id="userFilterSpan" style="display:none">' +
              '<input type="text" id="userNameBox" placeholder="Käyttäjätunnus"></span>';
          }
          html += '</label>';
      });
      return html;
    };

    var projectList = $('<div id="project-window" class="form-horizontal project-list"></div>').hide();
    projectList.append('<button class="close btn-close">x</button>');
    projectList.append('<div class="content">Tieosoiteprojektit</div>');
    projectList.append('<div class="content-new">' +
      headersToHtml() +
      '<div class="actions">' +
      '<button class="new btn btn-primary" style="margin-top:-5px;">Uusi tieosoiteprojekti</button></div>' +
      '</div>');
    projectList.append('<div id="project-list" style="width:820px; height:390px; overflow:auto;"></div>');
    projectList.append('<div class="content-footer">' +
      '<label class="tr-visible-checkbox checkbox"><input type="checkbox" name="OldAcceptedProjectsVisible" value="OldAcceptedProjectsVisible" id="OldAcceptedProjectsVisibleCheckbox">Näytä kaikki tieverkolle päivitetyt projektit</label>' +
      '<i id="sync" class="btn-icon btn-refresh fa fa-sync-alt" title="Päivitä lista"></i>' +
      '</div>');

    var staticFieldProjectName = function (dataField) {
      var field;
      field = '<div>' +
        '<label class="control-label-projects-list" style="width: 300px">' + dataField + '</label>' +
        '</div>';
      return field;
    };

    var staticFieldProjectList = function (dataField) {
      var field;
      field = '<div>' +
        '<label class="control-label-projects-list">' + dataField + '</label>' +
        '</div>';
      return field;
    };

    var pollProjects = null;

    function show() {
      $('.container').append('<div class="modal-overlay confirm-modal" id="projectList"><div class="modal-dialog"></div></div>');
      $('.modal-dialog').append(projectList.show());
      eventbus.trigger("roadAddressProject:deactivateAllSelections");
      bindEvents();
      fetchProjects();
      // start polling projects evey 60 seconds
      pollProjects = setInterval(fetchProjects, 60 * 1000);
    }

    function hide() {
      projectList.hide();
      eventbus.trigger("roadAddressProject:startAllInteractions");
      $('.modal-overlay').remove();
      clearInterval(pollProjects);
    }

    function fetchProjects() {
      projectCollection.getProjects();
    }

    var filterByUser = function () {
      var input = $('#userNameBox').val();
      var rows = $('#project-list').find('tr');
      if (input === "") {
        rows.show();
        return;
      }
      rows.hide();
      rows.each(function () {
        var label = $(this).find('.innerCreatedBy').find("label").text();
        if (label.toLowerCase().indexOf(input.toLowerCase()) !== -1)
          $(this).show();
      });
    };


    var userFilterVisibility = function () {
      var searchBox = $('#userFilterSpan');
      var textField = $('#userNameBox');
      if (filterBox.visible) {
        searchBox.show();
        if (textField.val() === "") {
          textField.val(applicationModel.getSessionUsername());
        }
      } else {
        textField.val("");
        searchBox.hide();
      }
      filterByUser();
    };

    function bindEvents() {

      eventbus.on('roadAddressProjects:fetched', function (projects) {
        projectArray = _.filter(projects, function (proj) {
          return proj.statusCode !== projectStatus.Deleted.value; //filter deleted projects out
        });
        createProjectList(projectArray);
        userFilterVisibility();
      });

      var createProjectList = function (projects) {
        var unfinishedProjects = _.filter(projects, function (proj) {
          if (proj.statusCode === projectStatus.Accepted.value) {
            var hoursInDay = 24;
            var millisecondsToHours = 1000 * 60 * 60;
            //check whether the show all projects checkbox is checked or the project has been saved to Road Network less than two days ago
            return $('#OldAcceptedProjectsVisibleCheckbox')[0].checked ||
                (new Date() - new Date(proj.dateModified.split('.').reverse().join('-'))) / millisecondsToHours < hoursInDay * 2;
          }
          return _.includes(statusToDisplay, proj.statusCode);
        });

        var sortedProjects = unfinishedProjects.sort(function (a, b) {
          var cmp = headers[orderBy.id].sortFunc(a, b);
          return (cmp === 0) ? a.name.localeCompare(b.name, 'fi') : cmp;
        });
        if (orderBy.reversed)
          sortedProjects.reverse();

        var triggerOpening = function (event, button) {
          $('#OldAcceptedProjectsVisibleCheckbox').prop('checked', false);
          if (button.length > 0 && button[0].className === "project-open btn btn-new-error") {
            projectCollection.reOpenProjectById(parseInt(event.currentTarget.value));
            eventbus.once("roadAddressProject:reOpenedProject", function (_successData) {
              openProjectSteps(event);
            });
          } else {
            openProjectSteps(event);
          }
        };

        var html = '<table style="table-layout: fixed; width: 100%;">';
        // eslint-disable-next-line no-negated-condition
        if (!_.isEmpty(sortedProjects)) {
          var uniqueId = 0;
          _.each(sortedProjects, function (proj) {
            var info = (proj.statusInfo) ? proj.statusInfo : 'Ei lisätietoja';
            html += '<tr id="' + uniqueId + '" class="project-item">' +
              '<td class="innerName" style="width: 270px;">' + staticFieldProjectName(proj.name) + '</td>' +
              '<td style="width: 60px; word-break: break-word" title="' + info + '">' + staticFieldProjectList(proj.elys) + '</td>' +
              '<td class="innerCreatedBy" style="width: 120px;" title="' + info + '">' + staticFieldProjectList(proj.createdBy) + '</td>' +
              '<td style="width: 120px;" title="' + info + '">' + staticFieldProjectList(proj.createdDate) + '</td>' +
              '<td style="width: 100px;" title="' + info + '">' + staticFieldProjectList(proj.statusDescription) + '</td>';
            switch (proj.statusCode) {
              case projectStatus.ErrorInViite.value:
                html += '<td id="innerOpenProjectButton"><button class="project-open btn btn-new-error" style="alignment: right; margin-bottom: 6px; margin-left: 25px" id="reopen-project-' + proj.id + '" value="' + proj.id + '" data-projectStatus="'+ proj.statusCode + '">Avaa uudelleen</button></td>' +
                  '</tr>';
                break;
              default:
                html += '<td id="innerOpenProjectButton"><button class="project-open btn btn-new" style="alignment: right; margin-bottom: 6px; margin-left: 50px" id="open-project-' + proj.id + '" value="' + proj.id + '" data-projectStatus="' + proj.statusCode + '">Avaa</button></td>' +
                  '</tr>';
            }
            uniqueId += 1;
          });
          html += '</table>';
          $('#project-list').html(html);
          $('[id*="open-project"]').click(function (event) {
            var button = $(this);
            if (parseInt(button.attr("data-projectStatus")) === projectStatus.InUpdateQueue.value ||
                parseInt(button.attr("data-projectStatus")) === projectStatus.UpdatingToRoadNetwork.value) {
              new GenericConfirmPopup("Projektin muokkaaminen ei ole mahdollista, koska sitä päivitetään tieverkolle. Haluatko avata sen?", {
                successCallback: function () {
                  clearInterval(pollProjects);
                  triggerOpening(event, button);
                },
                closeCallback: function () {
                }
              });
            } else {
              clearInterval(pollProjects);
              triggerOpening(event, button);
            }
          });
        } else {
          html += '</table>';
          $('#project-list').html(html);
        }
      };

      $('#filterUser').click(function () {
        filterBox.visible = !filterBox.visible;
        userFilterVisibility();
      });

      var openProjectSteps = function (event) {
        applicationModel.addSpinner();
        projectCollection.getProjectsWithLinksById(parseInt(event.currentTarget.value)).then(function (result) {
          setTimeout(function () {
          }, 0);
          eventbus.trigger('roadAddress:openProject', result);
          if (applicationModel.isReadOnly()) {
            $('.edit-mode-btn:visible').click();
          }
        });
      };

      /*
      User can sort project list by clicking the sort arrows next to column headers. By clicking same arrows again, user can reverse the order.
       */
      projectList.on('click', '[id^=sort]', function (event) {
        var eventId = event.target.id;
        _.forEach(Object.keys(headers), function(id) {
          var icon = 'fa-sort';
          if (id === eventId) {
            orderBy.reversed = orderBy.id === id && !orderBy.reversed;
            orderBy.id = id;
            icon = getIcon(id);
          }
          $('#' + id).removeClass('fa-sort fa-sort-up fa-sort-down').addClass(icon);
          // Update classes
        });
        // Create project list
        createProjectList(projectArray);
        filterByUser();
      });

      $('#OldAcceptedProjectsVisibleCheckbox').change(function () {
        createProjectList(projectArray);
        filterByUser();
      });

      projectList.on('click', 'button.cancel', function () {
        hide();
      });

      projectList.on('click', 'button.new', function () {
        $('#OldAcceptedProjectsVisibleCheckbox').prop('checked', false);
        $('.project-list').append('<div class="modal-overlay confirm-modal"><div class="modal-dialog"></div></div>');
        clearInterval(pollProjects);
        eventbus.trigger('roadAddress:newProject');
        if (applicationModel.isReadOnly()) {
          $('.edit-mode-btn:visible').click();
        }
      });

      projectList.on('click', 'button.close', function () {
        $('#project-list').find('table').hide();
        $('.project-item').remove();
        $('#OldAcceptedProjectsVisibleCheckbox').prop('checked', false);
        hide();
      });

      $('#userNameBox').keyup(function () {
        filterByUser();
      });

      projectList.on('click', '#sync', function () {
        fetchProjects();
      });
    }

    return {
      show: show,
      hide: hide,
      element: projectList,
      bindEvents: bindEvents
    };
  };
}(this));
