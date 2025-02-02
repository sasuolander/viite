/* eslint-disable no-unused-vars */
(function (root) {

  var generate = function () {
    return [{
      "statusCode": 1,
      "name": "test1",
      "statusDescription": "Keskeneräinen",
      "dateModified": "05.05.2017",
      "id": 455035,
      "createdBy": "silari",
      "additionalInfo": "Desc",
      "startDate": "05.05.2017",
      "modifiedBy": "-",
      "createdDate": "2017-05-05T17:23:52.000+03:00"
    }];
  };

  var generateProject = function () {
    return [{
      "statusCode": 1,
      "name": "Project Two",
      "statusDescription": "Keskeneräinen",
      "dateModified": "31.05.2017",
      "id": 454604,
      "createdBy": "silari",
      "additionalInfo": null,
      "startDate": "30.05.2017",
      "modifiedBy": "-",
      "createdDate": "2017-05-30T18:52:08.000+03:00"
    }];
  };

  var generateNormalLinkData = function () {
    return [
      [
        {
          "modifiedAt": "29.10.2015 15:34:02",
          "linkId": 5172099,
          "startAddressM": 3555,
          "roadNameFi": "Raimantie",
          "roadPartNumber": 1,
          "administrativeClassMML": "State",
          "segmentId": 190723,
          "municipalityCode": 749,
          "roadLinkType": 1,
          "constructionType": 0,
          "roadNumber": 16333,
          "trackCode": 0,
          "roadClass": 5,
          "sideCode": 3,
          "points": [{"x": 533002.493, "y": 6989057.755, "z": 111.33999999999651}, {
            "x": 533003.047,
            "y": 6989059.19,
            "z": 111.30999999999767
          }, {"x": 533009.396, "y": 6989075.615, "z": 110.94800000000396}, {
            "x": 533017.076,
            "y": 6989094.236,
            "z": 110.43499999999767
          }, {"x": 533028.861, "y": 6989120.633, "z": 109.42999999999302}, {
            "x": 533036.808,
            "y": 6989144.175,
            "z": 108.49800000000687
          }, {"x": 533041.456, "y": 6989165.199, "z": 107.57000000000698}, {
            "x": 533043.6549813771,
            "y": 6989195.388744326,
            "z": 106.4870091717406
          }],
          "id": 190723,
          "administrativeClassId": "1",
          "status": 99,
          "anomaly": 0,
          "startMValue": 0.0,
          "endAddressM": 3699,
          "endMValue": 144.847,
          "linkType": 99,
          "calibrationPoints": [],
          "mmlId": 318833294,
          "modifiedBy": "vvh_modified",
          "elyCode": 8,
          "discontinuity": 5,
          "roadLinkSource": 1
        }
      ]
    ];
  };

  var generateProjectLinks = function () {
    return [[{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717395,
      "roadName": "my road name",
      "startAddressM": 0,
      "roadPartNumber": 2,
      "administrativeClassMML": "Private",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 1129,
      "trackCode": 0,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 359416.688, "y": 6679137.536, "z": 51.994000000006054}, {
        "x": 359411.199,
        "y": 6679141.504,
        "z": 52.28900000000431
      }, {"x": 359399.268, "y": 6679148.46, "z": 52.64400000000023}, {
        "x": 359390.298,
        "y": 6679151.836,
        "z": 52.80899999999383
      }, {"x": 359381.88, "y": 6679152.437, "z": 52.83400000000256}, {
        "x": 359372.854,
        "y": 6679153.635,
        "z": 52.90700000000652
      }, {"x": 359366.212, "y": 6679157.257, "z": 52.97699999999895}, {
        "x": 359360.991,
        "y": 6679161.418,
        "z": 53.338000000003376
      }, {"x": 359357.779, "y": 6679167.85, "z": 53.929000000003725}, {
        "x": 359359.361,
        "y": 6679175.828,
        "z": 54.8859999999986
      }, {"x": 359362.287, "y": 6679184.794, "z": 56.1359999999986}, {
        "x": 359366.697,
        "y": 6679195.727,
        "z": 57.51499999999942
      }],
      "id": 0,
      "administrativeClassId": "3",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0,
      "endAddressM": 0,
      "endMValue": 98.49727136929418,
      "linkType": 3,
      "calibrationPoints": [],
      "mmlId": 362907913,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897070,
      "roadName": "my road name",
      "startAddressM": 0,
      "roadPartNumber": 0,
      "administrativeClassMML": "Private",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 359328.595, "y": 6679079.192, "z": 51.095000000001164}, {
        "x": 359334.912,
        "y": 6679072.341,
        "z": 51.04200000000128
      }, {"x": 359343.691, "y": 6679065.132, "z": 50.445999999996275}, {
        "x": 359349.82,
        "y": 6679061.249,
        "z": 50.30599999999686
      }, {"x": 359359.346, "y": 6679054.422, "z": 50.24300000000221}, {
        "x": 359368.355,
        "y": 6679047.366,
        "z": 50.12399999999616
      }, {"x": 359377.714, "y": 6679042.34, "z": 49.98399999999674}, {
        "x": 359388.797,
        "y": 6679037.931,
        "z": 50.070000000006985
      }, {"x": 359399.485, "y": 6679034.794, "z": 49.713000000003376}, {
        "x": 359407.611,
        "y": 6679033.364,
        "z": 49.255999999993946
      }, {"x": 359415.502, "y": 6679032.983, "z": 48.8579999999929}, {
        "x": 359422.556,
        "y": 6679033.796,
        "z": 48.461999999999534
      }, {"x": 359429.153, "y": 6679035.358, "z": 48.21700000000419}, {
        "x": 359435.245,
        "y": 6679037.386,
        "z": 48.11800000000221
      }, {"x": 359450.952, "y": 6679042.938, "z": 48.51600000000326}, {
        "x": 359467.343,
        "y": 6679047.674,
        "z": 49.10899999999674
      }, {"x": 359486.425, "y": 6679054.685, "z": 49.6359999999986}, {
        "x": 359506.694,
        "y": 6679065.124,
        "z": 49.36699999999837
      }, {"x": 359529.881, "y": 6679076.264, "z": 49.479000000006636}, {
        "x": 359552.35,
        "y": 6679087.497,
        "z": 49.93600000000151
      }, {"x": 359575.201, "y": 6679097.871, "z": 50.37300000000687}, {
        "x": 359577.041,
        "y": 6679098.681,
        "z": 50.45900000000256
      }],
      "id": 0,
      "administrativeClassId": "3",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0,
      "endAddressM": 0,
      "endMValue": 276.0386062647004,
      "linkType": 3,
      "calibrationPoints": [],
      "mmlId": 1210314960,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 150409,
      "startAddressM": 1664,
      "roadNameFi": "Siikajärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 4984,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11303,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 362992.018, "y": 6682294.04, "z": 66.68799999999464}, {
        "x": 362992.008,
        "y": 6682294.536,
        "z": 66.71199999999953
      }, {"x": 362994.049, "y": 6682307.773, "z": 67.25}, {
        "x": 362998.822,
        "y": 6682322.733,
        "z": 67.88700000000244
      }, {"x": 363004.249, "y": 6682337.797, "z": 68.5399999999936}, {
        "x": 363010.64,
        "y": 6682350.764,
        "z": 69.24899999999616
      }, {"x": 363018.403, "y": 6682364.177, "z": 69.82799999999406}, {
        "x": 363025.512,
        "y": 6682377.427,
        "z": 70.5789999999979
      }, {"x": 363031.856, "y": 6682391.047, "z": 71.44999999999709}, {
        "x": 363036.057,
        "y": 6682404.241,
        "z": 72.26399999999558
      }, {"x": 363038.563, "y": 6682418.598, "z": 72.9320000000007}, {
        "x": 363038.172,
        "y": 6682433.373,
        "z": 73.53900000000431
      }, {"x": 363035.118, "y": 6682448.385, "z": 74.00699999999779}, {
        "x": 363029.969,
        "y": 6682462.552,
        "z": 74.33900000000722
      }, {"x": 363023.87, "y": 6682476.563, "z": 74.57099999999627}, {
        "x": 363020.911,
        "y": 6682485.048,
        "z": 74.6820000000007
      }, {"x": 363015.104, "y": 6682500.469, "z": 74.9890000000014}, {
        "x": 363009.809,
        "y": 6682514.673,
        "z": 75.22999999999593
      }, {"x": 363005.351, "y": 6682528.958, "z": 75.36000000000058}, {
        "x": 363002.8761054283,
        "y": 6682540.077526318,
        "z": 75.3550002129825
      }],
      "id": 4984,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1925,
      "endMValue": 261.474,
      "roadNameSe": "Siikajärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083185,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 150429,
      "startAddressM": 1925,
      "roadNameFi": "Siikajärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 67508,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11303,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 362989.693, "y": 6682223.426, "z": 64.94199999999546}, {
        "x": 362989.771,
        "y": 6682225.43,
        "z": 64.94500000000698
      }, {"x": 362989.934, "y": 6682232.959, "z": 65.11100000000442}, {
        "x": 362990.137,
        "y": 6682242.325,
        "z": 65.2670000000071
      }, {"x": 362990.21, "y": 6682259.403, "z": 65.64100000000326}, {
        "x": 362990.698,
        "y": 6682276.354,
        "z": 66.0570000000007
      }, {"x": 362992.018, "y": 6682294.04, "z": 66.68799999999464}],
      "id": 67508,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1996,
      "endMValue": 70.676,
      "roadNameSe": "Siikajärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083941,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 150428,
      "startAddressM": 2031,
      "roadNameFi": "Siikajärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 68822,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11303,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 362981.914, "y": 6682045.684, "z": 63.270999999993364}, {
        "x": 362982.955,
        "y": 6682055.626,
        "z": 63.21899999999732
      }, {"x": 362983.156, "y": 6682057.443, "z": 63.263000000006286}, {
        "x": 362984.645,
        "y": 6682071.407,
        "z": 63.37699999999313
      }, {"x": 362985.435, "y": 6682082.855, "z": 63.513000000006286}, {
        "x": 362986.032,
        "y": 6682104.98,
        "z": 63.677999999999884
      }, {"x": 362985.98, "y": 6682119.237, "z": 63.84100000000035}, {
        "x": 362986.956,
        "y": 6682153.257,
        "z": 64.22400000000198
      }, {"x": 362987.424, "y": 6682172.047, "z": 64.403999999995}, {
        "x": 362987.943,
        "y": 6682182.533,
        "z": 64.49300000000221
      }, {"x": 362987.96, "y": 6682182.877, "z": 64.49599999999919}, {
        "x": 362988.23,
        "y": 6682188.348,
        "z": 64.45500000000175
      }],
      "id": 68822,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2174,
      "endMValue": 142.884,
      "roadNameSe": "Siikajärvivägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 362981.914, "y": 6682045.684, "z": 63.270999999993364}, "value": 2174}],
      "mmlId": 1019571274,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150430,
      "startAddressM": 1996,
      "roadNameFi": "Siikajärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 968,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11303,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 362988.23, "y": 6682188.348, "z": 64.45500000000175}, {
        "x": 362988.271,
        "y": 6682189.167,
        "z": 64.41599999999744
      }, {"x": 362989.693, "y": 6682223.426, "z": 64.94199999999546}],
      "id": 968,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2031,
      "endMValue": 35.109,
      "roadNameSe": "Siikajärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1019571286,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150401,
      "startAddressM": 10422,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 6,
      "administrativeClassMML": "State",
      "segmentId": 10556,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 363921.276, "y": 6681556.807, "z": 56.721999999994296}, {
        "x": 363910.747,
        "y": 6681560.48,
        "z": 56.737999999997555
      }, {"x": 363878.215, "y": 6681572.633, "z": 56.937000000005355}, {
        "x": 363844.852,
        "y": 6681584.744,
        "z": 56.987999999997555
      }, {"x": 363798.848, "y": 6681601.874, "z": 57.16800000000512}, {
        "x": 363754.698,
        "y": 6681618.279,
        "z": 57.74800000000687
      }, {"x": 363710.191, "y": 6681634.817, "z": 58.77499999999418}, {
        "x": 363664.209,
        "y": 6681651.906,
        "z": 59.99099999999453
      }, {"x": 363623.03414022806, "y": 6681666.87794901, "z": 60.910996866785226}],
      "id": 10556,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 10742,
      "endMValue": 317.909,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356084073,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150412,
      "startAddressM": 10742,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 6,
      "administrativeClassMML": "State",
      "segmentId": 16378,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 363623.034, "y": 6681666.878, "z": 60.91099999999278}, {
        "x": 363595.583,
        "y": 6681677.101,
        "z": 61.58100000000559
      }, {"x": 363559.232, "y": 6681690.938, "z": 62.43399999999383}, {
        "x": 363522.879,
        "y": 6681704.596,
        "z": 63.346000000005006
      }, {"x": 363485.943, "y": 6681718.744, "z": 64.13899999999558}, {
        "x": 363446.744,
        "y": 6681732.464,
        "z": 65.03399999999965
      }, {"x": 363429.795, "y": 6681738.466, "z": 65.096000000005}, {
        "x": 363423.8822459915,
        "y": 6681740.702906936,
        "z": 65.12099895995395
      }],
      "id": 16378,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 10956,
      "endMValue": 212.408,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082831,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499893902,
      "startAddressM": 11442,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 6,
      "administrativeClassMML": "State",
      "segmentId": 57283,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 363031.921, "y": 6682015.611, "z": 62.01499999999942}, {
        "x": 363022.636,
        "y": 6682021.12,
        "z": 62.44100000000617
      }, {"x": 362981.9142934901, "y": 6682045.683822963, "z": 63.27099401804783}],
      "id": 57283,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 11500,
      "endMValue": 58.353,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 362981.9204939435, "y": 6682045.680082776, "z": 63.27086763976756},
        "value": 11500
      }],
      "mmlId": 356082807,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499893874,
      "startAddressM": 10956,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 6,
      "administrativeClassMML": "State",
      "segmentId": 53568,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 363423.882, "y": 6681740.703, "z": 65.12099999999919}, {
        "x": 363394.747,
        "y": 6681752.215,
        "z": 64.69400000000314
      }, {"x": 363359.685, "y": 6681765.253, "z": 64.23399999999674}, {
        "x": 363325.333,
        "y": 6681781.003,
        "z": 63.67600000000675
      }, {"x": 363287.442, "y": 6681803.057, "z": 62.513999999995576}, {
        "x": 363250.671,
        "y": 6681830.304,
        "z": 61.12200000000303
      }, {"x": 363228.678, "y": 6681849.878, "z": 60.08100000000559}, {
        "x": 363206.954,
        "y": 6681870.869,
        "z": 58.8179999999993
      }, {"x": 363176.632, "y": 6681900.112, "z": 57.05100000000675}, {
        "x": 363154.694,
        "y": 6681922.175,
        "z": 56.27499999999418
      }, {"x": 363130.001, "y": 6681945.722, "z": 56.338000000003376}, {
        "x": 363106.643,
        "y": 6681966.046,
        "z": 57.328999999997905
      }, {"x": 363081.315, "y": 6681984.144, "z": 59.013999999995576}, {
        "x": 363053.839,
        "y": 6682002.472,
        "z": 60.861999999993714
      }, {"x": 363031.9211101923, "y": 6682015.610933944, "z": 62.01499420331399}],
      "id": 53568,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 11442,
      "endMValue": 483.817,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1742466693,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150448,
      "startAddressM": 9546,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 6,
      "administrativeClassMML": "State",
      "segmentId": 62702,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 364736.887, "y": 6681249.918, "z": 50.84399999999732}, {
        "x": 364718.786,
        "y": 6681258.092,
        "z": 50.96099999999569
      }, {"x": 364679.786, "y": 6681273.844, "z": 51.41899999999441}, {
        "x": 364643.878,
        "y": 6681287.696,
        "z": 51.979999999995925
      }, {"x": 364566.768, "y": 6681316.777, "z": 53.66899999999441}, {
        "x": 364392.487,
        "y": 6681381.692,
        "z": 55.75299999999697
      }, {"x": 364325.588, "y": 6681406.644, "z": 54.23500000000058}, {
        "x": 364248.176,
        "y": 6681435.32,
        "z": 55.65799999999581
      }, {"x": 364166.211, "y": 6681465.71, "z": 57.69400000000314}, {
        "x": 364128.593,
        "y": 6681479.484,
        "z": 57.66599999999744
      }, {"x": 364098.65, "y": 6681490.631, "z": 57.66099999999278}, {
        "x": 364020.109,
        "y": 6681519.676,
        "z": 56.979999999995925
      }, {"x": 363984.17, "y": 6681533.385, "z": 56.521999999997206}, {
        "x": 363942.472,
        "y": 6681549.413,
        "z": 56.57499999999709
      }, {"x": 363921.276, "y": 6681556.807, "z": 56.721999999994296}],
      "id": 62702,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 10422,
      "endMValue": 871.506,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356143196,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717049,
      "startAddressM": 1685,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 60827,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356885.064, "y": 6679747.087, "z": 56.254000000000815}, {
        "x": 356883.838,
        "y": 6679750.436,
        "z": 56.49300000000221
      }, {"x": 356877.224, "y": 6679773.436, "z": 57.6359999999986}, {
        "x": 356876.39900400554,
        "y": 6679777.78397889,
        "z": 57.51600058262698
      }],
      "id": 60827,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1717,
      "endMValue": 31.924,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888846,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717031,
      "startAddressM": 2420,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 58965,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356454.356, "y": 6680237.275, "z": 57.947000000000116}, {
        "x": 356449.659,
        "y": 6680240.718,
        "z": 57.570000000006985
      }, {"x": 356442.96337434935, "y": 6680245.415737351, "z": 57.13802415157958}],
      "id": 58965,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2434,
      "endMValue": 14.003,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064682,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717045,
      "startAddressM": 1440,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 39379,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357068.934, "y": 6679589.966, "z": 53.221999999994296}, {
        "x": 357043.411,
        "y": 6679611.112,
        "z": 53.812000000005355
      }, {"x": 357031.22920882, "y": 6679620.744834874, "z": 53.75200102849422}],
      "id": 39379,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1489,
      "endMValue": 48.675,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888936,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896896,
      "startAddressM": 2488,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 20108,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356397.487, "y": 6680274.358, "z": 53.69899999999325}, {
        "x": 356379.689,
        "y": 6680280.81,
        "z": 52.85199999999895
      }, {"x": 356352.264, "y": 6680288.642, "z": 52.404999999998836}, {
        "x": 356331.261,
        "y": 6680292.89,
        "z": 52.50100000000384
      }, {"x": 356309.4, "y": 6680296.353, "z": 53.245999999999185}, {
        "x": 356288.043,
        "y": 6680297.325,
        "z": 54.58900000000722
      }, {"x": 356265.22, "y": 6680297.746, "z": 56.21400000000722}, {
        "x": 356247.502,
        "y": 6680297.317,
        "z": 56.95200000000477
      }, {"x": 356233.76, "y": 6680296.645, "z": 57.18099999999686}, {
        "x": 356221.282,
        "y": 6680295.423,
        "z": 57.3579999999929
      }, {"x": 356204.863, "y": 6680291.849, "z": 57.44800000000396}, {
        "x": 356187.584,
        "y": 6680287.489,
        "z": 57.41800000000512
      }, {"x": 356165.768, "y": 6680282.925, "z": 57.38300000000163}, {
        "x": 356137.37,
        "y": 6680280.293,
        "z": 57.5570000000007
      }, {"x": 356111.235, "y": 6680279.701, "z": 58.26799999999639}, {
        "x": 356107.378,
        "y": 6680279.854,
        "z": 58.40099999999802
      }],
      "id": 20108,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2782,
      "endMValue": 294.674,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1748626701,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896900,
      "startAddressM": 2782,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 35338,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356107.378, "y": 6680279.854, "z": 58.40099999999802}, {
        "x": 356096.841,
        "y": 6680279.88,
        "z": 58.78100000000268
      }, {"x": 356087.313, "y": 6680280.679, "z": 59.13000000000466}, {
        "x": 356069.808,
        "y": 6680282.617,
        "z": 59.78900000000431
      }, {"x": 356036.903, "y": 6680287.541, "z": 60.55000000000291}, {
        "x": 356018.007,
        "y": 6680290.53,
        "z": 60.62200000000303
      }, {"x": 356014.404, "y": 6680291.5, "z": 60.66000000000349}, {
        "x": 355998.802,
        "y": 6680299.828,
        "z": 60.36599999999453
      }, {"x": 355998.231, "y": 6680300.343, "z": 60.336999999999534}],
      "id": 35338,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2895,
      "endMValue": 112.299,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063081,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717048,
      "startAddressM": 1610,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 25617,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356931.204, "y": 6679688.793, "z": 53.83599999999569}, {
        "x": 356930.615,
        "y": 6679689.196,
        "z": 53.836999999999534
      }, {"x": 356911.861, "y": 6679703.895, "z": 54.23500000000058}, {
        "x": 356896.486,
        "y": 6679723.581,
        "z": 54.92699999999604
      }, {"x": 356885.06417273637, "y": 6679747.086644516, "z": 56.253979931613316}],
      "id": 25617,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1685,
      "endMValue": 75.654,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888858,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896901,
      "startAddressM": 2895,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 55644,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355998.231, "y": 6680300.343, "z": 60.336999999999534}, {
        "x": 355992.741,
        "y": 6680307.054,
        "z": 60.02400000000489
      }, {"x": 355988.365, "y": 6680316.983, "z": 59.41599999999744}, {
        "x": 355984.337,
        "y": 6680335.678,
        "z": 58.14800000000105
      }, {"x": 355979.611, "y": 6680351.62, "z": 57.4600000000064}, {
        "x": 355978.388,
        "y": 6680355.111,
        "z": 57.370999999999185
      }],
      "id": 55644,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2953,
      "endMValue": 58.972,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1748539824,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717021,
      "startAddressM": 1977,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 49137,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356809.094, "y": 6679994.384, "z": 51.896999999997206}, {
        "x": 356791.757,
        "y": 6680006.668,
        "z": 52.58400000000256
      }, {"x": 356768.027, "y": 6680024.984, "z": 53.3920000000071}, {
        "x": 356744.137,
        "y": 6680044.676,
        "z": 54.28900000000431
      }, {"x": 356719.142, "y": 6680066.172, "z": 55.07799999999406}, {
        "x": 356697.842,
        "y": 6680087.076,
        "z": 55.76799999999639
      }, {"x": 356679.929, "y": 6680107.146, "z": 56.37799999999697}, {
        "x": 356661.09,
        "y": 6680123.516,
        "z": 56.85000000000582
      }, {"x": 356641.261, "y": 6680132.917, "z": 57.595000000001164}, {
        "x": 356614.901,
        "y": 6680137.39,
        "z": 58.88999999999942
      }, {"x": 356588.544, "y": 6680139.84, "z": 60.22299999999814}, {
        "x": 356564.088,
        "y": 6680143.252,
        "z": 61.4149999999936
      }, {"x": 356542.943, "y": 6680152.548, "z": 62.17699999999604}, {
        "x": 356525.676,
        "y": 6680170.378,
        "z": 63.12399999999616
      }, {"x": 356508.129, "y": 6680191.007, "z": 61.96899999999732}, {
        "x": 356498.758,
        "y": 6680202.135,
        "z": 60.84200000000419
      }, {"x": 356486.689, "y": 6680212.767, "z": 59.921000000002095}, {
        "x": 356474.603,
        "y": 6680221.858,
        "z": 59.24199999999837
      }, {"x": 356454.35624045745, "y": 6680237.274816905, "z": 57.94701537967973}],
      "id": 49137,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2420,
      "endMValue": 442.904,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063111,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717092,
      "startAddressM": 1194,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 14506,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357254.186, "y": 6679440.964, "z": 52.39100000000326}, {
        "x": 357250.203,
        "y": 6679456.187,
        "z": 52.60099999999511
      }, {"x": 357237.372, "y": 6679479.778, "z": 53.111999999993714}, {
        "x": 357221.732,
        "y": 6679499.003,
        "z": 53.05999999999767
      }, {"x": 357218.779, "y": 6679501.635, "z": 53.012000000002445}, {
        "x": 357212.474,
        "y": 6679506.27,
        "z": 52.75699999999779
      }, {"x": 357198.18, "y": 6679516.176, "z": 52.11999999999534}, {
        "x": 357172.05,
        "y": 6679532.006,
        "z": 51.45600000000559
      }, {"x": 357145.468, "y": 6679544.113, "z": 51.3920000000071}, {
        "x": 357116.487,
        "y": 6679557.963,
        "z": 51.49300000000221
      }, {"x": 357089.876, "y": 6679573.763, "z": 52.35099999999511}, {
        "x": 357068.934,
        "y": 6679589.966,
        "z": 53.221999999994296
      }],
      "id": 14506,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1440,
      "endMValue": 245.853,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888918,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717042,
      "startAddressM": 1717,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 65886,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356876.399, "y": 6679777.784, "z": 57.51600000000326}, {
        "x": 356874.877,
        "y": 6679789.016,
        "z": 56.8859999999986
      }],
      "id": 65886,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1729,
      "endMValue": 11.335,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888834,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717103,
      "startAddressM": 1007,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 66813,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357304.031, "y": 6679261.483, "z": 52.854000000006636}, {
        "x": 357301.205,
        "y": 6679267.857,
        "z": 53.07300000000396
      }, {"x": 357296.287, "y": 6679278.95, "z": 53.346000000005006}, {
        "x": 357285.283,
        "y": 6679305.353,
        "z": 53.05800000000454
      }, {"x": 357278.431, "y": 6679326.223, "z": 52.09200000000419}, {
        "x": 357268.788,
        "y": 6679351.418,
        "z": 51.30899999999383
      }, {"x": 357268.2261649311, "y": 6679352.838582977, "z": 51.295004108600594}],
      "id": 66813,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1105,
      "endMValue": 98.182,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889344,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717052,
      "startAddressM": 1506,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 53751,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357017.553, "y": 6679631.044, "z": 53.52599999999802}, {
        "x": 357016.944,
        "y": 6679631.485,
        "z": 53.50999999999476
      }, {"x": 357000.564, "y": 6679642.894, "z": 53.14999999999418}, {
        "x": 356976.518,
        "y": 6679659.799,
        "z": 53.14100000000326
      }, {"x": 356964.038, "y": 6679667.671, "z": 53.328999999997905}],
      "id": 53751,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1571,
      "endMValue": 64.863,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888882,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717040,
      "startAddressM": 1729,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 49776,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356874.877, "y": 6679789.016, "z": 56.8859999999986}, {
        "x": 356874.318,
        "y": 6679798.784,
        "z": 56.09299999999348
      }, {"x": 356876.211, "y": 6679820.526, "z": 53.995999999999185}, {
        "x": 356883.455,
        "y": 6679840.058,
        "z": 52.14900000000489
      }, {"x": 356893.684, "y": 6679857.696, "z": 51.080000000001746}, {
        "x": 356900.816,
        "y": 6679879.498,
        "z": 50.388999999995576
      }, {"x": 356898.079, "y": 6679902.576, "z": 50.213000000003376}, {
        "x": 356890.29,
        "y": 6679917.558,
        "z": 50.361999999993714
      }, {"x": 356883.458, "y": 6679927.16, "z": 50.55800000000454}, {
        "x": 356863.53,
        "y": 6679950.558,
        "z": 50.72599999999511
      }, {"x": 356837.518, "y": 6679973.707, "z": 51.07499999999709}, {
        "x": 356817.971,
        "y": 6679988.094,
        "z": 51.56100000000151
      }, {"x": 356809.094181801, "y": 6679994.38387118, "z": 51.896993118714875}],
      "id": 49776,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1977,
      "endMValue": 248.384,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063105,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896451,
      "startAddressM": 2953,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 33164,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355978.388, "y": 6680355.111, "z": 57.370999999999185}, {
        "x": 355974.571,
        "y": 6680364.88,
        "z": 57.16800000000512
      }, {"x": 355966.706, "y": 6680378.44, "z": 56.98500000000058}, {
        "x": 355955.183,
        "y": 6680396.227,
        "z": 56.80599999999686
      }, {"x": 355948.2790697769, "y": 6680408.8818721, "z": 56.87299932285589}],
      "id": 33164,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3015,
      "endMValue": 61.773,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 355948.27914434177, "y": 6680408.881735423, "z": 56.87299859923957},
        "value": 3015
      }],
      "mmlId": 356066392,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717101,
      "startAddressM": 1105,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 28704,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357268.226, "y": 6679352.839, "z": 51.294999999998254}, {
        "x": 357260.821,
        "y": 6679374.672,
        "z": 50.87799999999697
      }, {"x": 357257.788, "y": 6679397.827, "z": 51.09299999999348}, {
        "x": 357255.8,
        "y": 6679428.899,
        "z": 52.21799999999348
      }, {"x": 357254.18605195713, "y": 6679440.963611608, "z": 52.39099443086785}],
      "id": 28704,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1194,
      "endMValue": 89.715,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888948,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717043,
      "startAddressM": 1571,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 11463,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356964.038, "y": 6679667.671, "z": 53.328999999997905}, {
        "x": 356952.258,
        "y": 6679675.047,
        "z": 53.529999999998836
      }, {"x": 356931.204, "y": 6679688.793, "z": 53.83599999999569}],
      "id": 11463,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1610,
      "endMValue": 39.043,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888870,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717053,
      "startAddressM": 1489,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 50719,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357031.229, "y": 6679620.745, "z": 53.75199999999313}, {
        "x": 357017.55318610236,
        "y": 6679631.043859852,
        "z": 53.52600307539529
      }],
      "id": 50719,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1506,
      "endMValue": 17.12,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888894,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717029,
      "startAddressM": 2434,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 23125,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356442.963, "y": 6680245.416, "z": 57.138000000006286}, {
        "x": 356425.353,
        "y": 6680257.219,
        "z": 55.728000000002794
      }, {"x": 356409.746, "y": 6680267.507, "z": 54.43899999999849}, {
        "x": 356397.487,
        "y": 6680274.358,
        "z": 53.69899999999325
      }],
      "id": 23125,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2488,
      "endMValue": 53.936,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064688,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717112,
      "startAddressM": 596,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 18189,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357642.312, "y": 6679070.882, "z": 47.28500000000349}, {
        "x": 357629.31,
        "y": 6679077.983,
        "z": 47.25299999999697
      }, {"x": 357609.882, "y": 6679091.29, "z": 47.64299999999639}, {
        "x": 357596.436,
        "y": 6679103.008,
        "z": 48.58599999999569
      }, {"x": 357575.684, "y": 6679120.321, "z": 50.64400000000023}, {
        "x": 357553.518,
        "y": 6679136.821,
        "z": 52.171000000002095
      }, {"x": 357529.605, "y": 6679149.789, "z": 53.60899999999674}, {
        "x": 357505.018,
        "y": 6679155.115,
        "z": 55.15700000000652
      }, {"x": 357481.615054402, "y": 6679156.023997887, "z": 54.91500056254019}],
      "id": 18189,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 782,
      "endMValue": 186.638,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889368,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717093,
      "startAddressM": 420,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 21633,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357761.935, "y": 6678953.11, "z": 49.55199999999604}, {
        "x": 357751.017,
        "y": 6678975.459,
        "z": 49.10099999999511
      }, {"x": 357739.436, "y": 6678999.038, "z": 48.945999999996275}, {
        "x": 357725.533,
        "y": 6679022.63,
        "z": 48.76600000000326
      }, {"x": 357705.513, "y": 6679041.21, "z": 48.37200000000303}, {
        "x": 357681.171,
        "y": 6679054.975,
        "z": 47.85199999999895
      }, {"x": 357656.76, "y": 6679064.397, "z": 47.36800000000221}, {
        "x": 357642.31226690183,
        "y": 6679070.881880201,
        "z": 47.285001533285126
      }],
      "id": 21633,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 596,
      "endMValue": 175.807,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889362,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717157,
      "startAddressM": 19,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 35898,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 358090.957, "y": 6678781.081, "z": 51.73399999999674}, {
        "x": 358086.068,
        "y": 6678785.198,
        "z": 51.854999999995925
      }, {"x": 358080.641, "y": 6678789.175, "z": 51.96499999999651}],
      "id": 35898,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 32,
      "endMValue": 13.12,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362892493,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717134,
      "startAddressM": 32,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 5926,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 358080.641, "y": 6678789.175, "z": 51.96499999999651}, {
        "x": 358065.013,
        "y": 6678794.031,
        "z": 52.33599999999569
      }, {"x": 358043.848, "y": 6678796.663, "z": 52.83900000000722}, {
        "x": 358018.437,
        "y": 6678801.082,
        "z": 53.38499999999476
      }, {"x": 357995.807, "y": 6678803.08, "z": 54.03599999999278}, {
        "x": 357968.179,
        "y": 6678805.124,
        "z": 54.296000000002095
      }, {"x": 357947.325, "y": 6678805.892, "z": 54.31399999999849}, {
        "x": 357926.45,
        "y": 6678806.303,
        "z": 54.43300000000454
      }, {"x": 357917.898, "y": 6678806.421, "z": 54.47599999999511}, {
        "x": 357904.409,
        "y": 6678807.137,
        "z": 54.46700000000419
      }, {"x": 357883.261, "y": 6678812.147, "z": 54.312999999994645}, {
        "x": 357860.212,
        "y": 6678824.589,
        "z": 54.08400000000256
      }, {"x": 357843.778, "y": 6678842.371, "z": 53.687999999994645}, {
        "x": 357826.31612343236,
        "y": 6678865.611835718,
        "z": 52.66900720292503
      }],
      "id": 5926,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 311,
      "endMValue": 278.924,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1003764374,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717156,
      "startAddressM": 0,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 30888,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 358103.914, "y": 6678766.914, "z": 51.38199999999779}, {
        "x": 358090.957,
        "y": 6678781.081,
        "z": 51.73399999999674
      }],
      "id": 30888,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 19,
      "endMValue": 19.199,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 358103.914, "y": 6678766.914, "z": 51.38199999999779}, "value": 0}],
      "mmlId": 362892487,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717126,
      "startAddressM": 311,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 65819,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357826.316, "y": 6678865.612, "z": 52.66899999999441}, {
        "x": 357811.636,
        "y": 6678886.525,
        "z": 51.630999999993946
      }, {"x": 357794.1182388854, "y": 6678907.477714273, "z": 50.705012627462956}],
      "id": 65819,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 364,
      "endMValue": 52.862,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889308,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717125,
      "startAddressM": 364,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 49767,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 357794.118, "y": 6678907.478, "z": 50.705000000001746}, {
        "x": 357778.173,
        "y": 6678928.373,
        "z": 50.07600000000093
      }, {"x": 357763.79, "y": 6678949.789, "z": 49.62300000000687}, {
        "x": 357761.9352234516,
        "y": 6678953.109599955,
        "z": 49.55200855258941
      }],
      "id": 49767,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 420,
      "endMValue": 55.885,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889290,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717949,
      "startAddressM": 5549,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 5412,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355403.854, "y": 6680131.232, "z": 60.63300000000163}, {
        "x": 355409.917,
        "y": 6680136.67,
        "z": 60.56699999999546
      }, {"x": 355418.485, "y": 6680140.642, "z": 60.60199999999895}, {
        "x": 355435.437,
        "y": 6680144.97,
        "z": 60.828999999997905
      }, {"x": 355459.196, "y": 6680149.474, "z": 60.45900000000256}, {
        "x": 355474.284826784,
        "y": 6680152.830961463,
        "z": 59.93200604976028
      }],
      "id": 5412,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5624,
      "endMValue": 74.724,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063201,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717959,
      "startAddressM": 4739,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 32927,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355225.146, "y": 6679500.787, "z": 57.85199999999895}, {
        "x": 355213.805,
        "y": 6679496.974,
        "z": 57.37799999999697
      }, {"x": 355196.136, "y": 6679494.595, "z": 56.104999999995925}, {
        "x": 355182.311,
        "y": 6679494.373,
        "z": 54.49099999999453
      }, {"x": 355163.227, "y": 6679494.256, "z": 52.53399999999965}, {
        "x": 355144.222,
        "y": 6679494.664,
        "z": 51.87799999999697
      }, {"x": 355127.858, "y": 6679497.01, "z": 52.112999999997555}, {
        "x": 355112.775,
        "y": 6679500.249,
        "z": 52.62399999999616
      }, {"x": 355100.0623950749, "y": 6679503.761890829, "z": 53.00298822202154}],
      "id": 32927,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4866,
      "endMValue": 126.861,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362893218,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717962,
      "startAddressM": 5333,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 65999,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355389.379, "y": 6679916.929, "z": 59.479000000006636}, {
        "x": 355391.653,
        "y": 6679928.517,
        "z": 60.111000000004424
      }, {"x": 355392.644, "y": 6679941.228, "z": 60.97299999999814}, {
        "x": 355392.88,
        "y": 6679956.598,
        "z": 62.31100000000151
      }, {"x": 355393.366, "y": 6679972.319, "z": 63.58999999999651}, {
        "x": 355393.94,
        "y": 6679990.72,
        "z": 64.25900000000547
      }, {"x": 355395.495, "y": 6680011.213, "z": 64.36199999999371}],
      "id": 65999,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5427,
      "endMValue": 94.621,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356072197,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717950,
      "startAddressM": 5624,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 23639,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355474.285, "y": 6680152.831, "z": 59.9320000000007}, {
        "x": 355488.461,
        "y": 6680157.111,
        "z": 59.52700000000186
      }, {"x": 355504.806, "y": 6680164.604, "z": 59.39999999999418}, {
        "x": 355522.46093063167,
        "y": 6680177.872947864,
        "z": 59.63999905701469
      }],
      "id": 23639,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5679,
      "endMValue": 54.874,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063069,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717980,
      "startAddressM": 4627,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 68092,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355100.062, "y": 6679503.762, "z": 53.00299999999697}, {
        "x": 355089.488,
        "y": 6679507.201,
        "z": 53.187999999994645
      }, {"x": 355077.888, "y": 6679510.918, "z": 53.220000000001164}, {
        "x": 355057.342,
        "y": 6679515.799,
        "z": 53.052999999999884
      }, {"x": 355039.269, "y": 6679517.432, "z": 52.804999999993015}, {
        "x": 355021.524,
        "y": 6679518.099,
        "z": 52.671000000002095
      }, {"x": 355001.084, "y": 6679518.67, "z": 52.50900000000547}, {
        "x": 354990.344,
        "y": 6679518.78,
        "z": 52.46899999999732
      }],
      "id": 68092,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4739,
      "endMValue": 111.511,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891906,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717956,
      "startAddressM": 5679,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 1919,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355522.461, "y": 6680177.873, "z": 59.63999999999942}, {
        "x": 355530.629,
        "y": 6680187.738,
        "z": 59.82600000000093
      }, {"x": 355541.236, "y": 6680203.375, "z": 60.353000000002794}, {
        "x": 355552.6,
        "y": 6680223.381,
        "z": 61.163000000000466
      }, {"x": 355566.251, "y": 6680248.51, "z": 61.69100000000617}, {
        "x": 355576.199,
        "y": 6680265.42,
        "z": 61.570000000006985
      }, {"x": 355583.673, "y": 6680275.267, "z": 61.40099999999802}],
      "id": 1919,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5794,
      "endMValue": 115.29,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 942002562,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717960,
      "startAddressM": 5079,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 14596,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355344.184, "y": 6679673.836, "z": 59.096999999994296}, {
        "x": 355346.906,
        "y": 6679682.256,
        "z": 58.778000000005704
      }, {"x": 355349.867, "y": 6679694.845, "z": 58.63300000000163}, {
        "x": 355352.486,
        "y": 6679709.9,
        "z": 58.846000000005006
      }, {"x": 355353.579, "y": 6679725.955, "z": 59.14100000000326}, {
        "x": 355352.347,
        "y": 6679742.204,
        "z": 59.20900000000256
      }, {"x": 355350.128, "y": 6679759.499, "z": 58.846000000005006}, {
        "x": 355347.825,
        "y": 6679774.045,
        "z": 58.20200000000477
      }, {"x": 355345.852, "y": 6679789.596, "z": 57.463000000003376}, {
        "x": 355345.605,
        "y": 6679804.854,
        "z": 56.903000000005704
      }, {"x": 355348.138, "y": 6679817.158, "z": 56.96799999999348}, {
        "x": 355350.427,
        "y": 6679824.406,
        "z": 57.220000000001164
      }],
      "id": 14596,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5232,
      "endMValue": 152.713,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362892716,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897426,
      "startAddressM": 4952,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 33420,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355283.381, "y": 6679563.491, "z": 63.36999999999534}, {
        "x": 355290.351,
        "y": 6679574.55,
        "z": 63.55000000000291
      }, {"x": 355298.967, "y": 6679586.307, "z": 63.24300000000221}, {
        "x": 355304.73,
        "y": 6679593.789,
        "z": 62.85700000000361
      }, {"x": 355308.508, "y": 6679598.931, "z": 62.5570000000007}, {
        "x": 355316.451,
        "y": 6679612.083,
        "z": 61.82499999999709
      }, {"x": 355319.869, "y": 6679617.919, "z": 61.5570000000007}],
      "id": 33420,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5018,
      "endMValue": 65.601,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1227234653,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717963,
      "startAddressM": 5427,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 13639,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355395.495, "y": 6680011.213, "z": 64.36199999999371}, {
        "x": 355396.959,
        "y": 6680030.406,
        "z": 64.61199999999371
      }, {"x": 355398.81, "y": 6680049.748, "z": 64.63400000000547}, {
        "x": 355396.96,
        "y": 6680073.144,
        "z": 63.21799999999348
      }, {"x": 355395.606, "y": 6680091.417, "z": 62.33299999999872}, {
        "x": 355395.812,
        "y": 6680111.322,
        "z": 61.403000000005704
      }, {"x": 355398.427, "y": 6680122.652, "z": 60.937999999994645}, {
        "x": 355402.946,
        "y": 6680130.26,
        "z": 60.65899999999965
      }, {"x": 355403.85386173916, "y": 6680131.231851994, "z": 60.63300395901236}],
      "id": 13639,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5549,
      "endMValue": 122.184,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356071957,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896452,
      "startAddressM": 6178,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 52819,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355890.287, "y": 6680397.411, "z": 61.15600000000268}, {
        "x": 355903.93883514614,
        "y": 6680414.4147946695,
        "z": 60.69200560299948
      }],
      "id": 52819,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6200,
      "endMValue": 21.806,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065402,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717983,
      "startAddressM": 4567,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 35138,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 354990.344, "y": 6679518.78, "z": 52.46899999999732}, {
        "x": 354983.08,
        "y": 6679518.687,
        "z": 52.47400000000198
      }, {"x": 354966.195, "y": 6679518.13, "z": 52.163000000000466}, {
        "x": 354947.5,
        "y": 6679519.005,
        "z": 51.60000000000582
      }, {"x": 354930.993, "y": 6679519.154, "z": 51.078999999997905}, {
        "x": 354930.39415904076,
        "y": 6679519.137004514,
        "z": 51.060005044697085
      }],
      "id": 35138,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4627,
      "endMValue": 59.981,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891894,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716425,
      "startAddressM": 6200,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 15490,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355948.279, "y": 6680408.882, "z": 56.87300000000687}, {
        "x": 355939.919,
        "y": 6680412.451,
        "z": 57.187000000005355
      }, {"x": 355932.582, "y": 6680415.631, "z": 57.75900000000547}, {
        "x": 355922.515,
        "y": 6680417.9,
        "z": 58.74199999999837
      }, {"x": 355912.454, "y": 6680417.244, "z": 59.89999999999418}, {
        "x": 355903.9390081666,
        "y": 6680414.415002713,
        "z": 60.69199924040364
      }],
      "id": 15490,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6246,
      "endMValue": 46.461,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 355948.279, "y": 6680408.882, "z": 56.87300000000687}, "value": 6246}],
      "mmlId": 356063045,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897409,
      "startAddressM": 5794,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 18966,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355583.673, "y": 6680275.267, "z": 61.40099999999802}, {
        "x": 355586.11,
        "y": 6680277.872,
        "z": 61.37600000000384
      }, {"x": 355597.258, "y": 6680285.798, "z": 61.27400000000489}, {
        "x": 355611.175,
        "y": 6680289.439,
        "z": 61.255999999993946
      }, {"x": 355625.871, "y": 6680287.287, "z": 61.35099999999511}, {
        "x": 355646.875,
        "y": 6680277.594,
        "z": 61.720000000001164
      }, {"x": 355664.012, "y": 6680270.311, "z": 61.95200000000477}, {
        "x": 355679.439,
        "y": 6680268.716,
        "z": 62.30999999999767
      }, {"x": 355695.562, "y": 6680273.024, "z": 62.90099999999802}, {
        "x": 355717.285,
        "y": 6680285.495,
        "z": 63.94800000000396
      }, {"x": 355728.942, "y": 6680291.832, "z": 64.30299999999988}, {
        "x": 355748.865,
        "y": 6680295.282,
        "z": 64.38199999999779
      }, {"x": 355768.362, "y": 6680293.427, "z": 63.84900000000198}, {
        "x": 355782.885,
        "y": 6680288.57,
        "z": 63.304000000003725
      }, {"x": 355803.523, "y": 6680283.899, "z": 62.69199999999546}, {
        "x": 355815.914,
        "y": 6680285.914,
        "z": 62.513999999995576
      }, {"x": 355826.512, "y": 6680292.419, "z": 62.38499999999476}, {
        "x": 355833.072,
        "y": 6680301.54,
        "z": 62.228000000002794
      }, {"x": 355833.979, "y": 6680303.312, "z": 62.20699999999488}, {
        "x": 355842.596,
        "y": 6680323.451,
        "z": 61.85700000000361
      }, {"x": 355856.519, "y": 6680350.203, "z": 61.17399999999907}, {
        "x": 355868.704,
        "y": 6680368.099,
        "z": 60.861000000004424
      }, {"x": 355884.032, "y": 6680389.599, "z": 61.1079999999929}, {
        "x": 355890.287,
        "y": 6680397.411,
        "z": 61.15600000000268
      }],
      "id": 18966,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6178,
      "endMValue": 383.37,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1739845862,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717965,
      "startAddressM": 5232,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 18408,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355350.427, "y": 6679824.406, "z": 57.220000000001164}, {
        "x": 355353.132,
        "y": 6679831.661,
        "z": 57.54200000000128
      }, {"x": 355359.638, "y": 6679846.715, "z": 58.53100000000268}, {
        "x": 355365.776,
        "y": 6679859.508,
        "z": 58.95799999999872
      }, {"x": 355373.08, "y": 6679873.278, "z": 58.653000000005704}, {
        "x": 355376.043,
        "y": 6679879.933,
        "z": 58.554000000003725
      }, {"x": 355378.614, "y": 6679885.967, "z": 58.53599999999278}, {
        "x": 355384.711,
        "y": 6679899.357,
        "z": 58.74800000000687
      }, {"x": 355388.553, "y": 6679913.357, "z": 59.31100000000151}, {
        "x": 355389.3789002906,
        "y": 6679916.92856881,
        "z": 59.47897972011981
      }],
      "id": 18408,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5333,
      "endMValue": 100.659,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891972,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717973,
      "startAddressM": 4866,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 44898,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355225.146, "y": 6679500.787, "z": 57.85199999999895}, {
        "x": 355237.664,
        "y": 6679508.828,
        "z": 58.50800000000163
      }, {"x": 355248.228, "y": 6679516.88, "z": 59.262000000002445}, {
        "x": 355261.222,
        "y": 6679529.649,
        "z": 60.50699999999779
      }, {"x": 355274.096, "y": 6679547.676, "z": 62.25}, {
        "x": 355283.38099776424,
        "y": 6679563.490996192,
        "z": 63.369999730310084
      }],
      "id": 44898,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4952,
      "endMValue": 86.87,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891936,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717972,
      "startAddressM": 5018,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 52028,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11231,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355319.869, "y": 6679617.919, "z": 61.5570000000007}, {
        "x": 355326.996,
        "y": 6679629.727,
        "z": 61.09200000000419
      }, {"x": 355337.733, "y": 6679655.127, "z": 60.05800000000454}, {
        "x": 355344.1839288567,
        "y": 6679673.835793672,
        "z": 59.09701059815222
      }],
      "id": 52028,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5079,
      "endMValue": 61.158,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1004576308,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718108,
      "startAddressM": 780,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 23246,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11256,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355085.92, "y": 6676142.414, "z": 41.11800000000221}, {
        "x": 355073.374,
        "y": 6676150.915,
        "z": 40.68899999999849
      }, {"x": 355056.697, "y": 6676166.054, "z": 40.0679999999993}, {
        "x": 355040.879,
        "y": 6676183.566,
        "z": 39.471999999994296
      }, {"x": 355029.76, "y": 6676199.749, "z": 39.06600000000617}, {
        "x": 355029.419,
        "y": 6676200.334,
        "z": 39.05100000000675
      }, {"x": 355019.386, "y": 6676220.298, "z": 38.713000000003376}, {
        "x": 355012.061,
        "y": 6676235.653,
        "z": 38.60899999999674
      }],
      "id": 23246,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 901,
      "endMValue": 120.945,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891480,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718103,
      "startAddressM": 701,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 38969,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11256,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355012.061, "y": 6676235.653, "z": 38.60899999999674}, {
        "x": 355010.566,
        "y": 6676238.841,
        "z": 38.60599999999977
      }, {"x": 355002.703, "y": 6676259.095, "z": 38.604999999995925}, {
        "x": 354996.178,
        "y": 6676277.984,
        "z": 38.45200000000477
      }, {"x": 354989.265, "y": 6676296.044, "z": 38.2329999999929}, {
        "x": 354984.945,
        "y": 6676309.428,
        "z": 37.99000000000524
      }],
      "id": 38969,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 780,
      "endMValue": 78.634,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890706,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718100,
      "startAddressM": 901,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 63468,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11256,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355288.893, "y": 6676077.346, "z": 49.75900000000547}, {
        "x": 355283.403,
        "y": 6676080.745,
        "z": 49.71400000000722
      }, {"x": 355265.098, "y": 6676092.033, "z": 49.39800000000105}, {
        "x": 355247.256,
        "y": 6676102.767,
        "z": 48.78500000000349
      }, {"x": 355226.182, "y": 6676112.043, "z": 47.611999999993714}, {
        "x": 355207.447,
        "y": 6676119.321,
        "z": 46.15099999999802
      }, {"x": 355188.484, "y": 6676123.365, "z": 44.713000000003376}, {
        "x": 355169.105,
        "y": 6676125.742,
        "z": 43.61699999999837
      }, {"x": 355148.091, "y": 6676127.631, "z": 42.78200000000652}, {
        "x": 355127.37,
        "y": 6676129.331,
        "z": 42.18600000000151
      }, {"x": 355106.667, "y": 6676133.372, "z": 41.69100000000617}, {
        "x": 355090.251,
        "y": 6676140.035,
        "z": 41.25
      }, {"x": 355085.92, "y": 6676142.414, "z": 41.11800000000221}],
      "id": 63468,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1118,
      "endMValue": 216.464,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890772,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1720890,
      "startAddressM": 1430,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 51136,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11256,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355584.251, "y": 6675440.17, "z": 46.369000000006054}, {
        "x": 355583.471,
        "y": 6675443.412,
        "z": 46.60199999999895
      }, {"x": 355577.403, "y": 6675463.066, "z": 48.06500000000233}, {
        "x": 355569.528,
        "y": 6675480.182,
        "z": 49.270999999993364
      }, {"x": 355561.736, "y": 6675496.269, "z": 50.244000000006054}, {
        "x": 355551.88,
        "y": 6675515.321,
        "z": 51.129000000000815
      }, {"x": 355538.994, "y": 6675541.099, "z": 51.92399999999907}, {
        "x": 355529.729,
        "y": 6675565.089,
        "z": 52.4829999999929
      }, {"x": 355520.789, "y": 6675591.215, "z": 52.96099999999569}, {
        "x": 355513.54,
        "y": 6675617.419,
        "z": 53.44999999999709
      }, {"x": 355508.488, "y": 6675644.557, "z": 53.93300000000454}, {
        "x": 355506.402,
        "y": 6675671.894,
        "z": 54.34399999999732
      }, {"x": 355507.885, "y": 6675698.167, "z": 54.229999999995925}, {
        "x": 355510.954,
        "y": 6675729.707,
        "z": 53.05800000000454
      }, {"x": 355515.812, "y": 6675758.547, "z": 51.395000000004075}, {
        "x": 355519.476,
        "y": 6675784.975,
        "z": 49.78900000000431
      }, {"x": 355523.172, "y": 6675812.362, "z": 48.58599999999569}, {
        "x": 355527.212,
        "y": 6675845.252,
        "z": 48.112999999997555
      }, {"x": 355528.477, "y": 6675871.125, "z": 48.25999999999476}, {
        "x": 355527.884,
        "y": 6675881.377,
        "z": 48.421000000002095
      }, {"x": 355526.095, "y": 6675896.526, "z": 48.961999999999534}],
      "id": 51136,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1903,
      "endMValue": 472.559,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890658,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718099,
      "startAddressM": 1118,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 42600,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11256,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355526.095, "y": 6675896.526, "z": 48.961999999999534}, {
        "x": 355520.152,
        "y": 6675917.59,
        "z": 50.22500000000582
      }, {"x": 355509.688, "y": 6675940.369, "z": 51.8070000000007}, {
        "x": 355494.472,
        "y": 6675962.519,
        "z": 53.07799999999406
      }, {"x": 355479.839, "y": 6675976.912, "z": 53.5109999999986}, {
        "x": 355455.103,
        "y": 6675995.158,
        "z": 53.375
      }, {"x": 355426.342, "y": 6676010.298, "z": 52.39100000000326}, {
        "x": 355402.344,
        "y": 6676021.131,
        "z": 51.604000000006636
      }, {"x": 355390.515, "y": 6676025.472, "z": 51.21899999999732}, {
        "x": 355371.396,
        "y": 6676033.905,
        "z": 50.76900000000023
      }, {"x": 355352.812, "y": 6676042.141, "z": 50.44000000000233}, {
        "x": 355336.573,
        "y": 6676050.479,
        "z": 50.153000000005704
      }, {"x": 355317.564, "y": 6676060.41, "z": 49.971999999994296}, {
        "x": 355299.999,
        "y": 6676070.473,
        "z": 49.846000000005006
      }, {"x": 355288.893, "y": 6676077.346, "z": 49.75900000000547}],
      "id": 42600,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1430,
      "endMValue": 310.751,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890766,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 6552110,
      "startAddressM": 3163,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 42658,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356445.646, "y": 6675933.124, "z": 41.494000000006054}, {
        "x": 356444.608,
        "y": 6675938.739,
        "z": 41.471999999994296
      }, {"x": 356441.35, "y": 6675957.582, "z": 41.49700000000303}, {
        "x": 356437.973,
        "y": 6675978.303,
        "z": 41.65099999999802
      }, {"x": 356435.812, "y": 6675989.099, "z": 41.82799999999406}, {
        "x": 356432.285,
        "y": 6676004.478,
        "z": 42.10700000000361
      }, {"x": 356429.694, "y": 6676014.195, "z": 42.361000000004424}, {
        "x": 356428.542,
        "y": 6676018.856,
        "z": 42.52400000000489
      }, {"x": 356427.951, "y": 6676021.42, "z": 42.63499999999476}, {
        "x": 356419.284,
        "y": 6676044.327,
        "z": 43.138000000006286
      }, {"x": 356419.009, "y": 6676045.157, "z": 43.138999999995576}],
      "id": 42658,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3278,
      "endMValue": 115.471,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890448,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717761,
      "startAddressM": 3406,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 20836,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356386.443, "y": 6676168.843, "z": 40.31100000000151}, {
        "x": 356382.717,
        "y": 6676180.578,
        "z": 40.005999999993946
      }, {"x": 356372.267, "y": 6676201.051, "z": 38.77400000000489}, {
        "x": 356358.123,
        "y": 6676224.063,
        "z": 37.76600000000326
      }, {"x": 356344.382, "y": 6676243.222, "z": 38.0570000000007}, {
        "x": 356328.282,
        "y": 6676260.78,
        "z": 38.24099999999453
      }, {"x": 356311.912, "y": 6676277.04, "z": 38.12799999999697}, {
        "x": 356297.929,
        "y": 6676292.0,
        "z": 38.08999999999651
      }, {"x": 356285.878, "y": 6676308.004, "z": 37.95900000000256}, {
        "x": 356278.015,
        "y": 6676320.507,
        "z": 37.862999999997555
      }, {"x": 356267.146, "y": 6676340.483, "z": 37.44100000000617}, {
        "x": 356255.322,
        "y": 6676360.026,
        "z": 36.87399999999616
      }, {"x": 356239.524, "y": 6676383.892, "z": 36.19999999999709}, {
        "x": 356234.451,
        "y": 6676391.137,
        "z": 35.87399999999616
      }, {"x": 356229.051, "y": 6676398.525, "z": 35.41599999999744}, {
        "x": 356226.23,
        "y": 6676402.215,
        "z": 35.138000000006286
      }, {"x": 356213.173, "y": 6676418.396, "z": 34.361000000004424}, {
        "x": 356209.51,
        "y": 6676423.165,
        "z": 34.346000000005006
      }],
      "id": 20836,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3718,
      "endMValue": 311.713,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1211038221,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 6552109,
      "startAddressM": 3872,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 52491,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356168.836, "y": 6676565.357, "z": 41.14900000000489}, {
        "x": 356168.747,
        "y": 6676568.718,
        "z": 41.33400000000256
      }, {"x": 356165.851, "y": 6676590.813, "z": 42.33599999999569}, {
        "x": 356165.782,
        "y": 6676591.227,
        "z": 42.35099999999511
      }, {"x": 356162.7970583355, "y": 6676613.228570018, "z": 42.85399016993706}],
      "id": 52491,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3920,
      "endMValue": 48.269,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1224402314,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897222,
      "startAddressM": 3920,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 35975,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356162.797, "y": 6676613.229, "z": 42.854000000006636}, {
        "x": 356162.582,
        "y": 6676614.852,
        "z": 42.85599999999977
      }, {"x": 356161.724, "y": 6676626.415, "z": 43.00500000000466}, {
        "x": 356161.473,
        "y": 6676637.751,
        "z": 43.028999999994994
      }, {"x": 356163.613, "y": 6676657.921, "z": 42.604999999995925}, {
        "x": 356166.962,
        "y": 6676678.618,
        "z": 41.79300000000512
      }, {"x": 356171.52, "y": 6676701.952, "z": 40.75299999999697}, {
        "x": 356181.841,
        "y": 6676751.745,
        "z": 40.020000000004075
      }, {"x": 356186.799, "y": 6676775.397, "z": 40.65899999999965}, {
        "x": 356190.079,
        "y": 6676800.208,
        "z": 41.68300000000454
      }, {"x": 356190.075, "y": 6676817.483, "z": 42.470000000001164}, {
        "x": 356189.746,
        "y": 6676832.984,
        "z": 43.16000000000349
      }, {"x": 356188.3000043343, "y": 6676846.877958353, "z": 43.42299921166352}],
      "id": 35975,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4157,
      "endMValue": 236.388,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 356188.30026054644, "y": 6676846.875496519, "z": 43.42295261152682},
        "value": 4157
      }],
      "mmlId": 362890136,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717742,
      "startAddressM": 3010,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 33676,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356476.083, "y": 6675783.656, "z": 42.711999999999534}, {
        "x": 356475.943,
        "y": 6675784.056,
        "z": 42.70100000000093
      }, {"x": 356471.491, "y": 6675801.088, "z": 42.42399999999907}, {
        "x": 356468.177,
        "y": 6675816.809,
        "z": 42.20200000000477
      }, {"x": 356463.93, "y": 6675839.778, "z": 41.895999999993364}, {
        "x": 356458.889,
        "y": 6675865.772,
        "z": 41.705000000001746
      }, {"x": 356453.542, "y": 6675891.735, "z": 41.54799999999523}, {
        "x": 356448.854,
        "y": 6675915.769,
        "z": 41.48099999999977
      }, {"x": 356445.646, "y": 6675933.124, "z": 41.494000000006054}],
      "id": 33676,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3163,
      "endMValue": 152.575,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890442,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717726,
      "startAddressM": 3718,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 50485,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356209.51, "y": 6676423.165, "z": 34.346000000005006}, {
        "x": 356198.517,
        "y": 6676437.48,
        "z": 35.11699999999837
      }, {"x": 356183.846, "y": 6676457.47, "z": 36.84299999999348}, {
        "x": 356173.903,
        "y": 6676475.527,
        "z": 37.81900000000314
      }, {"x": 356167.57, "y": 6676499.21, "z": 38.604999999995925}, {
        "x": 356165.675,
        "y": 6676524.193,
        "z": 39.046000000002095
      }, {"x": 356167.694, "y": 6676546.974, "z": 39.9600000000064}, {
        "x": 356168.835995217,
        "y": 6676565.356923007,
        "z": 41.14899502018367
      }],
      "id": 50485,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3872,
      "endMValue": 154.317,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890148,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717763,
      "startAddressM": 3278,
      "roadNameFi": "Juusjärventie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 52817,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11255,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356419.009, "y": 6676045.157, "z": 43.138999999995576}, {
        "x": 356412.904,
        "y": 6676064.994,
        "z": 42.67299999999523
      }, {"x": 356406.933, "y": 6676086.582, "z": 42.05899999999383}, {
        "x": 356402.051,
        "y": 6676106.411,
        "z": 41.620999999999185
      }, {"x": 356401.499, "y": 6676108.651, "z": 41.572000000000116}, {
        "x": 356395.341,
        "y": 6676133.34,
        "z": 41.02599999999802
      }, {"x": 356389.366, "y": 6676157.525, "z": 40.56500000000233}, {
        "x": 356386.443,
        "y": 6676168.843,
        "z": 40.31100000000151
      }],
      "id": 52817,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3406,
      "endMValue": 127.929,
      "roadNameSe": "Juusjärvivägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890202,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150569,
      "startAddressM": 0,
      "roadNameFi": "Kolmirannantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 59728,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11289,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 361966.115, "y": 6682342.526, "z": 52.005999999993946}, {
        "x": 361973.198,
        "y": 6682350.665,
        "z": 52.028999999994994
      }, {"x": 361978.609, "y": 6682362.705, "z": 51.596999999994296}, {
        "x": 361981.207,
        "y": 6682378.853,
        "z": 51.08900000000722
      }, {"x": 361979.957, "y": 6682397.785, "z": 51.020999999993364}, {
        "x": 361975.754,
        "y": 6682412.474,
        "z": 51.12200000000303
      }, {"x": 361968.684, "y": 6682428.125, "z": 51.40200000000186}, {
        "x": 361954.783,
        "y": 6682456.921,
        "z": 52.612999999997555
      }, {"x": 361948.143, "y": 6682471.603, "z": 53.34200000000419}, {
        "x": 361943.591,
        "y": 6682486.485,
        "z": 53.828999999997905
      }, {"x": 361940.842, "y": 6682501.76, "z": 54.19800000000396}, {
        "x": 361940.142,
        "y": 6682506.012,
        "z": 54.28399999999965
      }, {"x": 361938.623, "y": 6682515.648, "z": 54.38499999999476}, {
        "x": 361938.5290314037,
        "y": 6682516.718642198,
        "z": 54.35101135878572
      }],
      "id": 59728,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 186,
      "endMValue": 186.082,
      "roadNameSe": "Kolmirantavägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 361966.115, "y": 6682342.526, "z": 52.005999999993946}, "value": 0}],
      "mmlId": 1019571299,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150564,
      "startAddressM": 186,
      "roadNameFi": "Kolmirannantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 58419,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11289,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 361938.529, "y": 6682516.719, "z": 54.35099999999511}, {
        "x": 361938.517,
        "y": 6682551.126,
        "z": 54.18399999999383
      }, {"x": 361938.402, "y": 6682552.112, "z": 54.20699999999488}],
      "id": 58419,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 221,
      "endMValue": 35.4,
      "roadNameSe": "Kolmirantavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1019571311,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150403,
      "startAddressM": 853,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 47652,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 364534.205, "y": 6681380.675, "z": 51.455000000001746}, {
        "x": 364499.957,
        "y": 6681393.069,
        "z": 51.778000000005704
      }, {"x": 364437.533, "y": 6681416.882, "z": 52.37799999999697}, {
        "x": 364364.621,
        "y": 6681445.949,
        "z": 53.1079999999929
      }, {"x": 364304.788, "y": 6681471.769, "z": 53.67399999999907}, {
        "x": 364247.22,
        "y": 6681497.023,
        "z": 54.28900000000431
      }, {"x": 364199.68, "y": 6681519.194, "z": 54.73099999999977}, {
        "x": 364154.73,
        "y": 6681540.593,
        "z": 55.179000000003725
      }, {"x": 364109.475, "y": 6681563.043, "z": 55.65799999999581}, {
        "x": 364068.475,
        "y": 6681584.208,
        "z": 56.0399999999936
      }, {"x": 364022.022, "y": 6681608.626, "z": 56.512000000002445}, {
        "x": 363988.873,
        "y": 6681626.985,
        "z": 56.84900000000198
      }, {"x": 363948.846, "y": 6681649.026, "z": 57.278999999994994}, {
        "x": 363900.288,
        "y": 6681676.886,
        "z": 57.84900000000198
      }, {"x": 363856.544, "y": 6681702.567, "z": 58.39999999999418}, {
        "x": 363814.473,
        "y": 6681728.805,
        "z": 59.03399999999965
      }, {"x": 363772.639, "y": 6681754.977, "z": 59.695000000006985}, {
        "x": 363761.252,
        "y": 6681762.372,
        "z": 59.85599999999977
      }, {"x": 363735.036, "y": 6681779.397, "z": 60.18399999999383}, {
        "x": 363691.365,
        "y": 6681808.397,
        "z": 60.921000000002095
      }, {"x": 363641.968, "y": 6681841.377, "z": 62.0850000000064}, {
        "x": 363602.566,
        "y": 6681867.556,
        "z": 62.91700000000128
      }],
      "id": 47652,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1908,
      "endMValue": 1054.318,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356148791,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.01.2016 23:00:05",
      "linkId": 150336,
      "startAddressM": 2624,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 41910,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 362979.681, "y": 6682219.554, "z": 68.89900000000489}, {
        "x": 362946.217,
        "y": 6682235.333,
        "z": 70.1359999999986
      }, {"x": 362897.07, "y": 6682257.928, "z": 70.6649999999936}, {
        "x": 362850.603,
        "y": 6682278.331,
        "z": 71.17299999999523
      }, {"x": 362802.369, "y": 6682299.306, "z": 71.6710000000021}, {
        "x": 362757.469,
        "y": 6682318.132,
        "z": 72.12799999999697
      }, {"x": 362714.904, "y": 6682335.306, "z": 72.46899999999732}, {
        "x": 362674.09,
        "y": 6682351.079,
        "z": 72.74899999999616
      }, {"x": 362625.776, "y": 6682368.377, "z": 72.9890000000014}, {
        "x": 362579.259,
        "y": 6682383.799,
        "z": 73.11699999999837
      }, {"x": 362544.725, "y": 6682393.861, "z": 73.14500000000407}, {
        "x": 362500.134,
        "y": 6682405.089,
        "z": 72.94899999999325
      }, {"x": 362457.614, "y": 6682413.365, "z": 72.56699999999546}, {
        "x": 362414.831,
        "y": 6682420.462,
        "z": 72.06399999999849
      }, {"x": 362377.22, "y": 6682425.431, "z": 71.42900000000373}, {
        "x": 362322.769,
        "y": 6682430.468,
        "z": 70.23699999999371
      }, {"x": 362279.092, "y": 6682434.618, "z": 69.20699999999488}, {
        "x": 362234.499,
        "y": 6682440.273,
        "z": 68.07700000000477
      }, {"x": 362191.514, "y": 6682446.997, "z": 66.87799999999697}, {
        "x": 362143.726,
        "y": 6682458.329,
        "z": 65.32799999999406
      }, {"x": 362096.722, "y": 6682472.537, "z": 63.62699999999313}, {
        "x": 362049.558,
        "y": 6682489.781,
        "z": 62.19000000000233
      }, {"x": 362004.96, "y": 6682508.571, "z": 61.1820000000007}, {
        "x": 361961.915,
        "y": 6682531.451,
        "z": 60.60199999999895
      }, {"x": 361951.159, "y": 6682537.722, "z": 60.47699999999895}],
      "id": 41910,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3710,
      "endMValue": 1084.722,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083821,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150416,
      "startAddressM": 1908,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 54728,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363602.566, "y": 6681867.556, "z": 62.91700000000128}, {
        "x": 363581.859,
        "y": 6681880.984,
        "z": 63.220000000001164
      }],
      "id": 54728,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1932,
      "endMValue": 24.68,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083869,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.01.2016 23:00:05",
      "linkId": 150406,
      "startAddressM": 1932,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 10276,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363581.859, "y": 6681880.984, "z": 63.220000000001164}, {
        "x": 363562.448,
        "y": 6681893.573,
        "z": 63.645000000004075
      }, {"x": 363536.064, "y": 6681910.277, "z": 64.10599999999977}, {
        "x": 363489.464,
        "y": 6681940.943,
        "z": 64.91000000000349
      }, {"x": 363439.482, "y": 6681971.562, "z": 65.6929999999993}, {
        "x": 363393.475,
        "y": 6681999.487,
        "z": 66.25400000000081
      }, {"x": 363355.932, "y": 6682021.177, "z": 66.75599999999395}, {
        "x": 363317.33,
        "y": 6682043.307,
        "z": 67.16899999999441
      }, {"x": 363279.667, "y": 6682064.941, "z": 67.55999999999767}, {
        "x": 363234.476,
        "y": 6682089.467,
        "z": 67.99000000000524
      }, {"x": 363186.413, "y": 6682115.597, "z": 68.35400000000664}, {
        "x": 363143.553,
        "y": 6682138.293,
        "z": 68.65200000000186
      }, {"x": 363107.886, "y": 6682156.266, "z": 68.83299999999872}, {
        "x": 363032.91,
        "y": 6682193.774,
        "z": 69.3640000000014
      }, {"x": 363000.991, "y": 6682209.415, "z": 69.65200000000186}],
      "id": 10276,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2601,
      "endMValue": 667.812,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083875,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150431,
      "startAddressM": 2601,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 23659,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363000.991, "y": 6682209.415, "z": 69.65200000000186}, {
        "x": 362996.057,
        "y": 6682211.832,
        "z": 69.81900000000314
      }, {"x": 362979.681, "y": 6682219.554, "z": 68.89900000000489}],
      "id": 23659,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2624,
      "endMValue": 23.6,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083797,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "07.04.2017 23:03:49",
      "linkId": 500130192,
      "startAddressM": 0,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 0,
      "administrativeClassMML": "State",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 356896.912, "y": 6677075.878, "z": 40.520000000004075}, {
        "x": 356914.651,
        "y": 6677079.102,
        "z": 40.50999999999476
      }, {"x": 356958.031, "y": 6677087.579, "z": 40.494000000006054}, {
        "x": 356987.729,
        "y": 6677093.704,
        "z": 40.47699999999895
      }, {"x": 357018.94, "y": 6677103.521, "z": 40.429000000003725}, {
        "x": 357048.761,
        "y": 6677118.572,
        "z": 40.38199999999779
      }, {"x": 357056.238, "y": 6677123.315, "z": 40.35199999999895}, {
        "x": 357075.025,
        "y": 6677137.355,
        "z": 40.29200000000128
      }, {"x": 357081.262, "y": 6677142.801, "z": 40.20299999999406}, {
        "x": 357097.031,
        "y": 6677159.043,
        "z": 40.13400000000547
      }, {"x": 357112.467, "y": 6677179.797, "z": 39.927999999999884}, {
        "x": 357127.615,
        "y": 6677206.442,
        "z": 39.6140000000014
      }, {"x": 357137.313, "y": 6677229.307, "z": 39.370999999999185}, {
        "x": 357143.41,
        "y": 6677244.205,
        "z": 39.044999999998254
      }, {"x": 357143.616, "y": 6677244.71, "z": 39.03900000000431}, {
        "x": 357155.478,
        "y": 6677271.13,
        "z": 38.54300000000512
      }, {"x": 357165.928, "y": 6677291.612, "z": 38.16700000000128}, {
        "x": 357180.25,
        "y": 6677313.703,
        "z": 37.61000000000058
      }, {"x": 357182.976, "y": 6677317.279, "z": 37.52400000000489}, {
        "x": 357195.39,
        "y": 6677333.463,
        "z": 37.09299999999348
      }, {"x": 357212.112, "y": 6677355.942, "z": 36.513000000006286}, {
        "x": 357222.003,
        "y": 6677368.947,
        "z": 36.17500000000291
      }, {"x": 357233.402, "y": 6677384.988, "z": 35.89100000000326}, {
        "x": 357240.916,
        "y": 6677396.616,
        "z": 35.853000000002794
      }, {"x": 357249.781, "y": 6677411.485, "z": 35.8579999999929}, {
        "x": 357261.398,
        "y": 6677433.368,
        "z": 36.14400000000023
      }, {"x": 357270.911, "y": 6677455.757, "z": 36.57300000000396}, {
        "x": 357274.218,
        "y": 6677465.435,
        "z": 36.828999999997905
      }],
      "id": 0,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 1,
      "startMValue": 0.0,
      "endAddressM": 0,
      "endMValue": 577.5899087673787,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889884,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 99
    }], [{
      "modifiedAt": "07.04.2017 23:03:49",
      "linkId": 500130191,
      "startAddressM": 0,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 0,
      "administrativeClassMML": "State",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 357481.615, "y": 6679156.024, "z": 54.9149999999936}, {
        "x": 357480.482,
        "y": 6679156.03,
        "z": 54.84799999999814
      }, {"x": 357448.094, "y": 6679157.035, "z": 53.20799999999872}, {
        "x": 357430.994,
        "y": 6679158.379,
        "z": 52.69400000000314
      }, {"x": 357419.438, "y": 6679159.852, "z": 52.40200000000186}, {
        "x": 357392.046,
        "y": 6679166.05,
        "z": 51.9429999999993
      }, {"x": 357364.743, "y": 6679178.019, "z": 51.64900000000489}, {
        "x": 357341.727,
        "y": 6679199.388,
        "z": 50.6140000000014
      }, {"x": 357323.075, "y": 6679225.334, "z": 50.96499999999651}, {
        "x": 357308.347,
        "y": 6679252.247,
        "z": 52.49800000000687
      }, {"x": 357304.031, "y": 6679261.483, "z": 52.854000000006636}],
      "id": 0,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 1,
      "startMValue": 0.0,
      "endAddressM": 0,
      "endMValue": 224.46966738614503,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889062,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 99
    }], [{
      "modifiedAt": "07.04.2017 23:03:49",
      "linkId": 500130194,
      "startAddressM": 0,
      "roadNameFi": "Solvikintie",
      "roadPartNumber": 0,
      "administrativeClassMML": "State",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 354984.945, "y": 6676309.428, "z": 37.99000000000524}, {
        "x": 354983.357,
        "y": 6676313.652,
        "z": 37.88000000000466
      }, {"x": 354974.647, "y": 6676330.029, "z": 37.44100000000617}, {
        "x": 354962.75,
        "y": 6676346.27,
        "z": 36.68899999999849
      }, {"x": 354948.991, "y": 6676362.514, "z": 35.720000000001164}, {
        "x": 354934.954,
        "y": 6676377.625,
        "z": 34.778999999994994
      }, {"x": 354918.58, "y": 6676392.731, "z": 33.804000000003725}, {
        "x": 354903.77,
        "y": 6676406.328,
        "z": 33.15099999999802
      }, {"x": 354891.228, "y": 6676417.547, "z": 32.84900000000198}, {
        "x": 354878.927,
        "y": 6676426.271,
        "z": 32.580000000001746
      }, {"x": 354863.921, "y": 6676434.196, "z": 32.44400000000314}, {
        "x": 354843.88,
        "y": 6676444.428,
        "z": 32.57300000000396
      }, {"x": 354824.057, "y": 6676453.981, "z": 32.84399999999732}, {
        "x": 354804.007,
        "y": 6676463.461,
        "z": 33.26600000000326
      }, {"x": 354783.505, "y": 6676475.019, "z": 33.93899999999849}, {
        "x": 354765.649,
        "y": 6676485.601,
        "z": 34.52700000000186
      }, {"x": 354748.917, "y": 6676495.72, "z": 35.169999999998254}, {
        "x": 354736.919,
        "y": 6676505.484,
        "z": 35.67200000000594
      }, {"x": 354727.078, "y": 6676512.523, "z": 36.120999999999185}],
      "id": 0,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 1,
      "startMValue": 0.0,
      "endAddressM": 0,
      "endMValue": 334.46646574955116,
      "roadNameSe": "Solviksvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890790,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 99
    }], [{
      "modifiedAt": "07.04.2017 23:03:49",
      "linkId": 500130201,
      "startAddressM": 0,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 0,
      "administrativeClassMML": "State",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 355603.474, "y": 6676807.33, "z": 36.86500000000524}, {
        "x": 355597.304,
        "y": 6676807.315,
        "z": 36.86000000000058
      }, {"x": 355591.48, "y": 6676807.379, "z": 36.8579999999929}, {
        "x": 355560.805,
        "y": 6676807.711,
        "z": 36.802999999999884
      }, {"x": 355532.258, "y": 6676807.614, "z": 36.604999999995925}],
      "id": 0,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 1,
      "startMValue": 0.0,
      "endAddressM": 0,
      "endMValue": 71.21833125863967,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890910,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 99
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716426,
      "startAddressM": 0,
      "roadNameFi": "Kylmäläntie",
      "roadPartNumber": 0,
      "administrativeClassMML": "State",
      "segmentId": 0,
      "municipalityCode": 257,
      "roadLinkType": 0,
      "constructionType": 0,
      "roadNumber": 0,
      "trackCode": 99,
      "roadClass": 99,
      "sideCode": 99,
      "points": [{"x": 355903.939, "y": 6680414.415, "z": 60.69199999999546}, {
        "x": 355912.439,
        "y": 6680433.03,
        "z": 60.08299999999872
      }, {"x": 355919.091, "y": 6680450.873, "z": 59.812000000005355}, {
        "x": 355922.699,
        "y": 6680463.578,
        "z": 59.75
      }, {"x": 355924.117, "y": 6680475.286, "z": 59.778999999994994}, {
        "x": 355926.576,
        "y": 6680484.216,
        "z": 60.02599999999802
      }],
      "id": 0,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 1,
      "startMValue": 0.0,
      "endAddressM": 0,
      "endMValue": 73.76976117774907,
      "roadNameSe": "Kylmälävägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063057,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 99
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716427,
      "startAddressM": 0,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 47196,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355948.279, "y": 6680408.882, "z": 56.87300000000687}, {
        "x": 355933.964,
        "y": 6680458.882,
        "z": 57.986999999993714
      }, {"x": 355926.57603302744, "y": 6680484.2158867465, "z": 60.025990884811605}],
      "id": 47196,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 79,
      "endMValue": 78.398,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 355948.279, "y": 6680408.882, "z": 56.87300000000687}, "value": 0}],
      "mmlId": 356063051,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716396,
      "startAddressM": 1231,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 64804,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355065.42, "y": 6681048.382, "z": 49.95600000000559}, {
        "x": 355080.487,
        "y": 6681049.056,
        "z": 49.68899999999849
      }, {"x": 355099.762, "y": 6681051.854, "z": 49.29300000000512}, {
        "x": 355117.459,
        "y": 6681057.021,
        "z": 48.711999999999534
      }],
      "id": 64804,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1284,
      "endMValue": 52.995,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064784,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716430,
      "startAddressM": 79,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 18269,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355926.576, "y": 6680484.216, "z": 60.02599999999802}, {
        "x": 355926.344,
        "y": 6680484.978,
        "z": 60.07300000000396
      }, {"x": 355920.7950194165, "y": 6680506.764923765, "z": 60.906997081760736}],
      "id": 18269,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 102,
      "endMValue": 23.279,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063063,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716409,
      "startAddressM": 387,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 67375,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355821.895, "y": 6680768.267, "z": 44.02599999999802}, {
        "x": 355803.666,
        "y": 6680798.58,
        "z": 44.14900000000489
      }, {"x": 355784.995, "y": 6680830.3, "z": 45.044999999998254}, {
        "x": 355776.233,
        "y": 6680843.693,
        "z": 45.70100000000093
      }, {"x": 355767.793, "y": 6680852.936, "z": 46.278000000005704}, {
        "x": 355756.059,
        "y": 6680860.006,
        "z": 46.92200000000594
      }, {"x": 355742.17, "y": 6680864.414, "z": 47.39299999999639}, {
        "x": 355727.65,
        "y": 6680867.689,
        "z": 47.6929999999993
      }, {"x": 355709.815, "y": 6680871.55, "z": 47.513999999995576}, {
        "x": 355678.717,
        "y": 6680877.612,
        "z": 46.18399999999383
      }, {"x": 355653.569, "y": 6680883.55, "z": 43.978000000002794}, {
        "x": 355625.723,
        "y": 6680890.941,
        "z": 41.197000000000116
      }, {"x": 355596.28729396954, "y": 6680899.634913175, "z": 39.30101893485555}],
      "id": 67375,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 667,
      "endMValue": 279.13,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065576,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716401,
      "startAddressM": 1407,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 58388,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 354850.609, "y": 6681066.862, "z": 52.221999999994296}, {
        "x": 354852.431,
        "y": 6681066.416,
        "z": 52.18099999999686
      }, {"x": 354865.506, "y": 6681066.683, "z": 51.89400000000023}, {
        "x": 354901.894,
        "y": 6681075.813,
        "z": 51.479000000006636
      }, {"x": 354920.13, "y": 6681080.332, "z": 51.53200000000652}, {
        "x": 354936.348,
        "y": 6681080.816,
        "z": 51.59299999999348
      }, {"x": 354948.168, "y": 6681079.209, "z": 51.58900000000722}],
      "id": 58388,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1507,
      "endMValue": 99.411,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064820,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716404,
      "startAddressM": 232,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 5964,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355895.802, "y": 6680633.217, "z": 53.572000000000116}, {
        "x": 355886.645,
        "y": 6680643.862,
        "z": 52.697000000000116
      }, {"x": 355879.382, "y": 6680652.65, "z": 51.611999999993714}, {
        "x": 355872.459,
        "y": 6680663.278,
        "z": 50.211999999999534
      }, {"x": 355864.192, "y": 6680681.725, "z": 47.887000000002445}, {
        "x": 355855.514,
        "y": 6680705.879,
        "z": 46.01799999999639
      }, {"x": 355848.801, "y": 6680721.145, "z": 45.19999999999709}, {
        "x": 355840.374,
        "y": 6680737.069,
        "z": 44.645999999993364
      }, {"x": 355830.677, "y": 6680753.778, "z": 44.278000000005704}, {
        "x": 355821.895,
        "y": 6680768.267,
        "z": 44.02599999999802
      }],
      "id": 5964,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 387,
      "endMValue": 154.962,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356062889,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716378,
      "startAddressM": 667,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 9427,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355596.287, "y": 6680899.635, "z": 39.30100000000675}, {
        "x": 355593.202,
        "y": 6680900.802,
        "z": 39.17600000000675
      }, {"x": 355568.607, "y": 6680911.578, "z": 38.697000000000116}, {
        "x": 355551.112,
        "y": 6680922.431,
        "z": 38.546000000002095
      }, {"x": 355537.487, "y": 6680933.302, "z": 38.50900000000547}, {
        "x": 355528.95,
        "y": 6680945.489,
        "z": 38.52400000000489
      }, {"x": 355523.539, "y": 6680962.562, "z": 38.83599999999569}, {
        "x": 355519.564,
        "y": 6680981.504,
        "z": 39.33299999999872
      }, {"x": 355515.837, "y": 6680997.341, "z": 39.67399999999907}, {
        "x": 355511.318,
        "y": 6681008.571,
        "z": 39.78900000000431
      }, {"x": 355502.422, "y": 6681021.354, "z": 39.76600000000326}, {
        "x": 355492.28,
        "y": 6681032.259,
        "z": 39.70799999999872
      }, {"x": 355479.147, "y": 6681041.174, "z": 39.72599999999511}, {
        "x": 355464.346,
        "y": 6681047.456,
        "z": 39.94800000000396
      }, {"x": 355449.887, "y": 6681048.5, "z": 40.236000000004424}, {
        "x": 355433.797,
        "y": 6681047.686,
        "z": 40.637000000002445
      }, {"x": 355409.932, "y": 6681046.063, "z": 40.97500000000582}, {
        "x": 355383.867,
        "y": 6681047.056,
        "z": 40.66800000000512
      }, {"x": 355355.168, "y": 6681049.714, "z": 40.74899999999616}, {
        "x": 355328.988,
        "y": 6681052.453,
        "z": 41.1820000000007
      }, {"x": 355300.15, "y": 6681056.278, "z": 41.9890000000014}, {
        "x": 355264.608,
        "y": 6681061.57,
        "z": 42.97299999999814
      }, {"x": 355228.515, "y": 6681067.467, "z": 43.87699999999313}, {
        "x": 355206.23,
        "y": 6681070.802,
        "z": 44.64299999999639
      }, {"x": 355171.46, "y": 6681072.541, "z": 45.95699999999488}, {
        "x": 355164.93106972874,
        "y": 6681071.784008085,
        "z": 46.17099771451348
      }],
      "id": 9427,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1181,
      "endMValue": 512.377,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356066320,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716421,
      "startAddressM": 129,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 11250,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355915.676, "y": 6680533.333, "z": 60.721999999994296}, {
        "x": 355914.776,
        "y": 6680540.088,
        "z": 60.45100000000093
      }, {"x": 355911.602, "y": 6680578.663, "z": 57.17500000000291}, {
        "x": 355910.589,
        "y": 6680585.04,
        "z": 56.63499999999476
      }, {"x": 355909.7, "y": 6680590.934, "z": 56.20799999999872}, {
        "x": 355906.179,
        "y": 6680607.971,
        "z": 55.038000000000466
      }, {"x": 355900.021, "y": 6680622.975, "z": 54.11800000000221}, {
        "x": 355895.8020708771,
        "y": 6680633.21682794,
        "z": 53.57200917252501
      }],
      "id": 11250,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 232,
      "endMValue": 102.63,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065546,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716397,
      "startAddressM": 1181,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 2732,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 355117.459, "y": 6681057.021, "z": 48.711999999999534}, {
        "x": 355133.76,
        "y": 6681063.654,
        "z": 47.8070000000007
      }, {"x": 355150.649, "y": 6681069.268, "z": 46.78599999999278}, {
        "x": 355164.930618028,
        "y": 6681071.783932709,
        "z": 46.171016448173155
      }],
      "id": 2732,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1231,
      "endMValue": 49.898,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356066314,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716400,
      "startAddressM": 1284,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 44467,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355065.42, "y": 6681048.382, "z": 49.95600000000559}, {
        "x": 355046.136,
        "y": 6681048.93,
        "z": 50.16800000000512
      }, {"x": 355025.364, "y": 6681052.01, "z": 50.16800000000512}],
      "id": 44467,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1324,
      "endMValue": 40.291,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064796,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716429,
      "startAddressM": 102,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 2163,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355920.795, "y": 6680506.765, "z": 60.90700000000652}, {
        "x": 355915.676,
        "y": 6680533.333,
        "z": 60.721999999994296
      }],
      "id": 2163,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 129,
      "endMValue": 27.057,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065552,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716394,
      "startAddressM": 1324,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 32864,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355025.364, "y": 6681052.01, "z": 50.16800000000512}, {
        "x": 355007.148,
        "y": 6681055.135,
        "z": 49.96600000000035
      }, {"x": 354990.722, "y": 6681060.16, "z": 50.18099999999686}, {
        "x": 354977.574,
        "y": 6681066.717,
        "z": 50.70299999999406
      }, {"x": 354962.486, "y": 6681074.28, "z": 51.39400000000023}, {
        "x": 354951.215,
        "y": 6681078.459,
        "z": 51.546000000002095
      }, {"x": 354948.168, "y": 6681079.209, "z": 51.58900000000722}],
      "id": 32864,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1407,
      "endMValue": 82.388,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 941835320,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716868,
      "startAddressM": 4276,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 45720,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356313.976, "y": 6682117.001, "z": 44.23399999999674}, {
        "x": 356349.402,
        "y": 6682130.071,
        "z": 44.69000000000233
      }, {"x": 356373.584, "y": 6682139.396, "z": 44.63000000000466}],
      "id": 45720,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4340,
      "endMValue": 63.678,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065690,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716329,
      "startAddressM": 3779,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 7700,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356092.269, "y": 6682479.496, "z": 47.02400000000489}, {
        "x": 356074.704,
        "y": 6682511.104,
        "z": 48.955000000001746
      }, {"x": 356070.965, "y": 6682516.73, "z": 49.278000000005704}],
      "id": 7700,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3822,
      "endMValue": 42.916,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356054055,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716830,
      "startAddressM": 4977,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 37021,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356898.016, "y": 6682442.384, "z": 51.91000000000349}, {
        "x": 356906.32,
        "y": 6682456.957,
        "z": 51.370999999999185
      }, {"x": 356918.045, "y": 6682477.354, "z": 50.69199999999546}, {
        "x": 356928.272,
        "y": 6682493.999,
        "z": 49.994000000006054
      }, {"x": 356937.879, "y": 6682508.014, "z": 48.99099999999453}, {
        "x": 356945.777,
        "y": 6682517.289,
        "z": 48.68099999999686
      }, {"x": 356955.015, "y": 6682526.836, "z": 49.16700000000128}, {
        "x": 356965.249,
        "y": 6682536.338,
        "z": 50.24099999999453
      }, {"x": 356975.286, "y": 6682545.183, "z": 51.20799999999872}, {
        "x": 356987.84,
        "y": 6682554.58,
        "z": 51.971999999994296
      }, {"x": 357001.706, "y": 6682565.004, "z": 52.52499999999418}, {
        "x": 357016.1087856739,
        "y": 6682575.302846745,
        "z": 52.802995863177635
      }],
      "id": 37021,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5158,
      "endMValue": 180.372,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064700,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716335,
      "startAddressM": 3961,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 23008,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356188.589, "y": 6682354.243, "z": 48.00199999999313}, {
        "x": 356179.468,
        "y": 6682361.574,
        "z": 47.22400000000198
      }, {"x": 356172.487, "y": 6682367.472, "z": 46.93399999999383}],
      "id": 23008,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3982,
      "endMValue": 20.841,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065768,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716863,
      "startAddressM": 4181,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 58726,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356277.295, "y": 6682122.755, "z": 45.39800000000105}, {
        "x": 356273.775,
        "y": 6682131.32,
        "z": 45.48099999999977
      }, {"x": 356271.377, "y": 6682141.914, "z": 45.49899999999616}, {
        "x": 356267.027,
        "y": 6682172.593,
        "z": 45.304000000003725
      }, {"x": 356266.38904874236, "y": 6682176.959666368, "z": 45.27200244475303}],
      "id": 58726,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4236,
      "endMValue": 55.521,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053965,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716824,
      "startAddressM": 3982,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 26801,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356233.442, "y": 6682285.546, "z": 50.820000000006985}, {
        "x": 356227.864,
        "y": 6682302.852,
        "z": 50.59100000000035
      }, {"x": 356222.701, "y": 6682316.487, "z": 50.3350000000064}, {
        "x": 356216.855,
        "y": 6682328.195,
        "z": 50.10899999999674
      }, {"x": 356208.279, "y": 6682337.589, "z": 49.84399999999732}, {
        "x": 356188.5892568465,
        "y": 6682354.242782757,
        "z": 48.00202402799131
      }],
      "id": 26801,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4066,
      "endMValue": 84.357,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065762,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716844,
      "startAddressM": 4737,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 32693,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356733.471, "y": 6682290.787, "z": 50.512000000002445}, {
        "x": 356760.508,
        "y": 6682304.695,
        "z": 49.794999999998254
      }, {"x": 356784.684, "y": 6682316.896, "z": 50.21499999999651}],
      "id": 32693,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4795,
      "endMValue": 57.485,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356071945,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716814,
      "startAddressM": 4340,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 59854,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356373.584, "y": 6682139.396, "z": 44.63000000000466}, {
        "x": 356442.249,
        "y": 6682165.39,
        "z": 44.25699999999779
      }, {"x": 356490.016, "y": 6682181.236, "z": 44.187999999994645}, {
        "x": 356518.022,
        "y": 6682190.563,
        "z": 43.929999999993015
      }, {"x": 356534.144, "y": 6682198.492, "z": 44.02599999999802}, {
        "x": 356548.77,
        "y": 6682207.459,
        "z": 44.478000000002794
      }, {"x": 356561.191, "y": 6682218.36, "z": 44.68099999999686}, {
        "x": 356573.144,
        "y": 6682229.054,
        "z": 45.40099999999802
      }, {"x": 356586.294, "y": 6682240.839, "z": 46.5170000000071}, {
        "x": 356597.653,
        "y": 6682248.578,
        "z": 47.54099999999744
      }, {"x": 356610.969, "y": 6682253.935, "z": 48.604000000006636}, {
        "x": 356624.867,
        "y": 6682256.972,
        "z": 49.836999999999534
      }, {"x": 356639.868, "y": 6682259.042, "z": 51.46899999999732}, {
        "x": 356651.996,
        "y": 6682259.933,
        "z": 52.61699999999837
      }, {"x": 356667.296, "y": 6682261.337, "z": 53.419999999998254}],
      "id": 59854,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4664,
      "endMValue": 323.603,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065696,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716866,
      "startAddressM": 4066,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 9443,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356266.389, "y": 6682176.96, "z": 45.271999999997206}, {
        "x": 356265.646,
        "y": 6682182.054,
        "z": 45.262000000002445
      }, {"x": 356265.297, "y": 6682184.217, "z": 45.25500000000466}, {
        "x": 356261.425,
        "y": 6682212.46,
        "z": 46.03599999999278
      }, {"x": 356253.969, "y": 6682232.038, "z": 47.23399999999674}, {
        "x": 356243.441,
        "y": 6682258.022,
        "z": 48.97299999999814
      }, {"x": 356233.442, "y": 6682285.546, "z": 50.820000000006985}],
      "id": 9443,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4181,
      "endMValue": 114.116,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053971,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716848,
      "startAddressM": 4683,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 19840,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356685.295, "y": 6682266.957, "z": 53.08199999999488}, {
        "x": 356690.476,
        "y": 6682269.339,
        "z": 52.887000000002445
      }, {"x": 356706.7429722639, "y": 6682277.097986771, "z": 52.332000946300624}],
      "id": 19840,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4707,
      "endMValue": 23.725,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053857,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716835,
      "startAddressM": 4923,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 60877,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356886.905, "y": 6682389.461, "z": 51.713000000003376}, {
        "x": 356891.106,
        "y": 6682410.386,
        "z": 51.97400000000198
      }, {"x": 356893.076, "y": 6682419.895, "z": 52.10599999999977}, {
        "x": 356898.016,
        "y": 6682442.384,
        "z": 51.91000000000349
      }],
      "id": 60877,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4977,
      "endMValue": 54.079,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 926480239,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716322,
      "startAddressM": 3700,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 65479,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356070.965, "y": 6682516.73, "z": 49.278000000005704}, {
        "x": 356061.502,
        "y": 6682528.312,
        "z": 49.679000000003725
      }, {"x": 356047.547, "y": 6682538.871, "z": 49.661999999996624}, {
        "x": 356030.267,
        "y": 6682547.447,
        "z": 49.578999999997905
      }, {"x": 356017.501, "y": 6682552.182, "z": 49.38199999999779}, {
        "x": 356003.8874739432,
        "y": 6682555.397888042,
        "z": 49.074010722375164
      }],
      "id": 65479,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3779,
      "endMValue": 79.351,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356066020,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716833,
      "startAddressM": 4864,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 50423,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356845.599, "y": 6682349.852, "z": 50.60000000000582}, {
        "x": 356861.858,
        "y": 6682359.051,
        "z": 50.91099999999278
      }, {"x": 356874.216, "y": 6682368.573, "z": 51.20299999999406}, {
        "x": 356880.92098130815,
        "y": 6682378.125973369,
        "z": 51.42499938112409
      }],
      "id": 50423,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4910,
      "endMValue": 45.953,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063513,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716334,
      "startAddressM": 3885,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 2573,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356172.487, "y": 6682367.472, "z": 46.93399999999383}, {
        "x": 356159.957,
        "y": 6682380.495,
        "z": 46.83999999999651
      }, {"x": 356146.947, "y": 6682395.531, "z": 46.89299999999639}, {
        "x": 356133.401,
        "y": 6682411.154,
        "z": 46.629000000000815
      }, {"x": 356123.69826417946, "y": 6682424.824627786, "z": 46.357007405633055}],
      "id": 2573,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3961,
      "endMValue": 75.397,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 941990007,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716851,
      "startAddressM": 4707,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 44629,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356706.743, "y": 6682277.098, "z": 52.33199999999488}, {
        "x": 356728.011,
        "y": 6682287.991,
        "z": 50.86500000000524
      }, {"x": 356733.471, "y": 6682290.787, "z": 50.512000000002445}],
      "id": 44629,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4737,
      "endMValue": 30.03,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356071939,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716869,
      "startAddressM": 4236,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 23610,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356313.976, "y": 6682117.001, "z": 44.23399999999674}, {
        "x": 356307.201,
        "y": 6682115.999,
        "z": 44.270999999993364
      }, {"x": 356290.234, "y": 6682115.183, "z": 44.721000000005006}, {
        "x": 356282.886,
        "y": 6682117.522,
        "z": 45.17600000000675
      }, {"x": 356277.295, "y": 6682122.755, "z": 45.39800000000105}],
      "id": 23610,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4276,
      "endMValue": 39.205,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053947,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716332,
      "startAddressM": 3822,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 55750,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 356123.698, "y": 6682424.825, "z": 46.35700000000361}, {
        "x": 356099.066,
        "y": 6682467.537,
        "z": 46.68899999999849
      }, {"x": 356097.028, "y": 6682471.021, "z": 46.778999999994994}, {
        "x": 356095.709,
        "y": 6682473.277,
        "z": 46.81900000000314
      }, {"x": 356092.744, "y": 6682478.603, "z": 46.986999999993714}, {
        "x": 356092.2692064346,
        "y": 6682479.495611903,
        "z": 47.02398391983404
      }],
      "id": 55750,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3885,
      "endMValue": 63.062,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065792,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716838,
      "startAddressM": 4910,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 9863,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356880.921, "y": 6682378.126, "z": 51.42500000000291}, {
        "x": 356886.905,
        "y": 6682389.461,
        "z": 51.713000000003376
      }],
      "id": 9863,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4923,
      "endMValue": 12.818,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063519,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716850,
      "startAddressM": 4664,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 65818,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356667.296, "y": 6682261.337, "z": 53.419999999998254}, {
        "x": 356678.36,
        "y": 6682264.121,
        "z": 53.330000000001746
      }, {"x": 356685.2946666742, "y": 6682266.95686369, "z": 53.08201191993544}],
      "id": 65818,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4683,
      "endMValue": 18.901,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053869,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716841,
      "startAddressM": 4795,
      "roadNameFi": "Eerikinkartanontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 7330,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356784.684, "y": 6682316.896, "z": 50.21499999999651}, {
        "x": 356815.975,
        "y": 6682334.277,
        "z": 50.02599999999802
      }, {"x": 356845.5989676666, "y": 6682349.8519830005, "z": 50.599999373508574}],
      "id": 7330,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4864,
      "endMValue": 69.263,
      "roadNameSe": "Eriksgårdsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356071921,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716366,
      "startAddressM": 2956,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 4742,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355374.675, "y": 6682249.307, "z": 44.05199999999604}, {
        "x": 355396.779,
        "y": 6682279.961,
        "z": 46.262000000002445
      }, {"x": 355420.2058945777, "y": 6682312.473853691, "z": 49.119987138897265}],
      "id": 4742,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3034,
      "endMValue": 77.866,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053917,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716384,
      "startAddressM": 2050,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 28678,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 354935.039, "y": 6681477.275, "z": 49.880999999993946}, {
        "x": 354956.536,
        "y": 6681496.503,
        "z": 48.713000000003376
      }, {"x": 354984.95, "y": 6681521.249, "z": 48.604000000006636}, {
        "x": 355009.027,
        "y": 6681542.234,
        "z": 49.8179999999993
      }, {"x": 355028.371, "y": 6681559.894, "z": 51.41000000000349}, {
        "x": 355033.243,
        "y": 6681565.729,
        "z": 51.653999999994994
      }],
      "id": 28678,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2182,
      "endMValue": 132.254,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 941865373,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716376,
      "startAddressM": 2279,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 15189,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355074.248, "y": 6681650.548, "z": 51.73500000000058}, {
        "x": 355075.995,
        "y": 6681660.53,
        "z": 51.85099999999511
      }],
      "id": 15189,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2290,
      "endMValue": 10.134,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356072011,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716311,
      "startAddressM": 3034,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 30076,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355420.206, "y": 6682312.474, "z": 49.11999999999534}, {
        "x": 355444.349,
        "y": 6682339.278,
        "z": 53.71799999999348
      }, {"x": 355455.256, "y": 6682350.047, "z": 55.70799999999872}, {
        "x": 355492.283,
        "y": 6682382.115,
        "z": 60.53100000000268
      }, {"x": 355503.251, "y": 6682389.921, "z": 61.79399999999441}, {
        "x": 355525.206969368,
        "y": 6682403.941980438,
        "z": 63.634997431517135
      }],
      "id": 30076,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3174,
      "endMValue": 139.898,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065288,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716338,
      "startAddressM": 3251,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 49840,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355588.349, "y": 6682447.074, "z": 60.53599999999278}, {
        "x": 355599.458,
        "y": 6682456.734,
        "z": 59.82600000000093
      }, {"x": 355609.39, "y": 6682466.18, "z": 58.729000000006636}, {
        "x": 355617.526,
        "y": 6682475.067,
        "z": 57.64100000000326
      }, {"x": 355636.562, "y": 6682495.166, "z": 55.45100000000093}, {
        "x": 355652.69089643157,
        "y": 6682506.307928454,
        "z": 54.28800746791858
      }],
      "id": 49840,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3339,
      "endMValue": 87.763,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356066685,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716379,
      "startAddressM": 1878,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 31879,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 354813.186, "y": 6681356.758, "z": 51.7899999999936}, {
        "x": 354825.295,
        "y": 6681372.697,
        "z": 52.11000000000058
      }, {"x": 354837.607, "y": 6681386.926, "z": 51.403999999994994}, {
        "x": 354852.641,
        "y": 6681401.449,
        "z": 50.529999999998836
      }, {"x": 354867.864, "y": 6681415.547, "z": 49.82300000000396}, {
        "x": 354899.821,
        "y": 6681444.923,
        "z": 49.25299999999697
      }, {"x": 354935.0389016963, "y": 6681477.274909697, "z": 49.88099824706277}],
      "id": 31879,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2050,
      "endMValue": 171.714,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065018,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716375,
      "startAddressM": 2290,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 31021,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355075.995, "y": 6681660.53, "z": 51.85099999999511}, {
        "x": 355084.171,
        "y": 6681688.617,
        "z": 51.49499999999534
      }, {"x": 355089.218, "y": 6681701.611, "z": 51.50999999999476}],
      "id": 31021,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2333,
      "endMValue": 43.193,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356072023,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716387,
      "startAddressM": 2182,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 42484,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355033.243, "y": 6681565.729, "z": 51.653999999994994}, {
        "x": 355039.104,
        "y": 6681571.488,
        "z": 51.73500000000058
      }, {"x": 355049.196, "y": 6681583.271, "z": 51.471999999994296}, {
        "x": 355057.2608313302,
        "y": 6681593.1087942505,
        "z": 51.365002237782015
      }],
      "id": 42484,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2219,
      "endMValue": 36.452,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065012,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716348,
      "startAddressM": 3174,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 34081,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355525.207, "y": 6682403.942, "z": 63.63499999999476}, {
        "x": 355560.86,
        "y": 6682426.456,
        "z": 61.88300000000163
      }, {"x": 355588.349, "y": 6682447.074, "z": 60.53599999999278}],
      "id": 34081,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3251,
      "endMValue": 76.529,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356065282,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716370,
      "startAddressM": 2394,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 51347,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355108.089, "y": 6681759.395, "z": 50.18300000000454}, {
        "x": 355121.605,
        "y": 6681788.075,
        "z": 48.78399999999965
      }, {"x": 355136.368, "y": 6681816.149, "z": 47.74499999999534}, {
        "x": 355152.551,
        "y": 6681849.118,
        "z": 47.72400000000198
      }, {"x": 355166.35290786176, "y": 6681874.332831671, "z": 48.115997383113296}],
      "id": 51347,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2523,
      "endMValue": 128.896,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356054139,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716389,
      "startAddressM": 2219,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 10748,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355057.261, "y": 6681593.109, "z": 51.36500000000524}, {
        "x": 355065.313,
        "y": 6681608.984,
        "z": 51.346999999994296
      }, {"x": 355069.592, "y": 6681624.025, "z": 51.380999999993946}, {
        "x": 355072.049,
        "y": 6681636.993,
        "z": 51.52300000000105
      }, {"x": 355074.2479948622, "y": 6681650.54796833, "z": 51.734999504678434}],
      "id": 10748,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2279,
      "endMValue": 60.369,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356054169,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716342,
      "startAddressM": 3339,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 45473,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355652.691, "y": 6682506.308, "z": 54.288000000000466}, {
        "x": 355671.174,
        "y": 6682515.934,
        "z": 53.22699999999895
      }, {"x": 355686.783, "y": 6682521.9, "z": 52.5850000000064}],
      "id": 45473,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3377,
      "endMValue": 37.55,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356066679,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716360,
      "startAddressM": 2523,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 31786,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355166.353, "y": 6681874.333, "z": 48.11599999999453}, {
        "x": 355183.024,
        "y": 6681908.347,
        "z": 47.82300000000396
      }, {"x": 355197.121, "y": 6681938.077, "z": 48.03599999999278}, {
        "x": 355209.608,
        "y": 6681968.151,
        "z": 48.603000000002794
      }, {"x": 355225.317, "y": 6682009.468, "z": 47.04799999999523}, {
        "x": 355238.677,
        "y": 6682046.824,
        "z": 45.911999999996624
      }, {"x": 355253.619, "y": 6682079.023, "z": 45.80599999999686}, {
        "x": 355272.948,
        "y": 6682108.719,
        "z": 44.638000000006286
      }, {"x": 355305.074, "y": 6682154.901, "z": 44.93099999999686}, {
        "x": 355332.67,
        "y": 6682193.59,
        "z": 44.66400000000431
      }, {"x": 355354.074, "y": 6682222.987, "z": 43.679000000003725}, {
        "x": 355374.6749957983,
        "y": 6682249.306994632,
        "z": 44.05199992392123
      }],
      "id": 31786,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2956,
      "endMValue": 431.718,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356054133,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716373,
      "startAddressM": 2333,
      "roadNameFi": "Sjökullantie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 58592,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11233,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 355089.218, "y": 6681701.611, "z": 51.50999999999476}, {
        "x": 355095.934,
        "y": 6681722.865,
        "z": 51.41400000000431
      }, {"x": 355102.937, "y": 6681744.106, "z": 50.804999999993015}, {
        "x": 355108.0889359019,
        "y": 6681759.394809783,
        "z": 50.18300773855483
      }],
      "id": 58592,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2394,
      "endMValue": 60.789,
      "roadNameSe": "Sjökullavägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356072017,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718566,
      "startAddressM": 7571,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 13329,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 361431.059, "y": 6676584.636, "z": 29.698000000003958}, {
        "x": 361439.855,
        "y": 6676593.076,
        "z": 29.504000000000815
      }, {"x": 361447.403, "y": 6676601.831, "z": 29.25199999999313}, {
        "x": 361454.667,
        "y": 6676611.746,
        "z": 29.08400000000256
      }, {"x": 361460.496, "y": 6676622.865, "z": 29.077999999994063}, {
        "x": 361467.016,
        "y": 6676635.792,
        "z": 29.088000000003376
      }, {"x": 361476.285, "y": 6676655.629, "z": 29.19999999999709}, {
        "x": 361486.997,
        "y": 6676677.897,
        "z": 28.960999999995693
      }, {"x": 361507.676, "y": 6676717.54, "z": 26.694000000003143}, {
        "x": 361519.329,
        "y": 6676739.612,
        "z": 25.793999999994412
      }, {"x": 361533.333, "y": 6676763.075, "z": 24.773000000001048}, {
        "x": 361536.997,
        "y": 6676769.133,
        "z": 24.65799999999581
      }, {"x": 361540.79, "y": 6676773.805, "z": 24.52400000000489}, {
        "x": 361545.515,
        "y": 6676776.453,
        "z": 24.410000000003492
      }],
      "id": 13329,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7797,
      "endMValue": 225.19,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 361545.51418384956, "y": 6676776.4525426105, "z": 24.410019691252867},
        "value": 7797
      }],
      "mmlId": 362906510,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717557,
      "startAddressM": 6494,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 43877,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 360714.645, "y": 6675911.229, "z": 45.49700000000303}, {
        "x": 360726.673,
        "y": 6675914.583,
        "z": 45.80599999999686
      }, {"x": 360742.764689948, "y": 6675919.261909848, "z": 46.17999279396257}],
      "id": 43877,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6523,
      "endMValue": 29.245,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904094,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718607,
      "startAddressM": 6523,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 44962,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 360742.765, "y": 6675919.262, "z": 46.179999999993015}, {
        "x": 360751.184,
        "y": 6675921.913,
        "z": 46.25199999999313
      }, {"x": 360764.119, "y": 6675927.021, "z": 46.27599999999802}, {
        "x": 360777.654,
        "y": 6675937.196,
        "z": 46.375
      }, {"x": 360790.815, "y": 6675951.996, "z": 46.44999999999709}, {
        "x": 360803.944,
        "y": 6675969.252,
        "z": 46.19400000000314
      }, {"x": 360815.64, "y": 6675981.59, "z": 45.695000000006985}, {
        "x": 360829.797,
        "y": 6675994.079,
        "z": 45.179000000003725
      }, {"x": 360841.497, "y": 6676003.182, "z": 44.895999999993364}, {
        "x": 360858.124,
        "y": 6676016.123,
        "z": 44.50699999999779
      }, {"x": 360874.131, "y": 6676030.162, "z": 44.21400000000722}, {
        "x": 360885.685,
        "y": 6676043.522,
        "z": 44.28500000000349
      }, {"x": 360896.496, "y": 6676059.602, "z": 44.828999999997905}, {
        "x": 360905.926,
        "y": 6676075.55,
        "z": 45.9320000000007
      }, {"x": 360913.782, "y": 6676091.017, "z": 47.197000000000116}, {
        "x": 360920.408,
        "y": 6676107.245,
        "z": 48.32799999999406
      }, {"x": 360924.682, "y": 6676123.02, "z": 48.9320000000007}, {
        "x": 360929.647,
        "y": 6676141.343,
        "z": 49.14100000000326
      }, {"x": 360934.396, "y": 6676158.717, "z": 49.070000000006985}, {
        "x": 360937.152,
        "y": 6676167.184,
        "z": 49.06600000000617
      }, {"x": 360939.012, "y": 6676174.347, "z": 49.08599999999569}, {
        "x": 360941.833,
        "y": 6676184.366,
        "z": 49.220000000001164
      }, {"x": 360946.011, "y": 6676195.21, "z": 49.50500000000466}, {
        "x": 360952.34,
        "y": 6676214.976,
        "z": 49.986999999993714
      }, {"x": 360957.985, "y": 6676232.872, "z": 50.486000000004424}, {
        "x": 360960.3368990396,
        "y": 6676242.38159178,
        "z": 50.69899085689928
      }],
      "id": 44962,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6929,
      "endMValue": 405.651,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362908093,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718593,
      "startAddressM": 6994,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 454656,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 360987.833, "y": 6676299.708, "z": 50.221000000005006}, {
        "x": 360998.044,
        "y": 6676306.666,
        "z": 49.22400000000198
      }, {"x": 361017.709, "y": 6676316.564, "z": 47.6649999999936}, {
        "x": 361036.511,
        "y": 6676324.6,
        "z": 46.54700000000594
      }, {"x": 361055.553, "y": 6676334.102, "z": 45.46400000000722}, {
        "x": 361072.977,
        "y": 6676344.018,
        "z": 44.75699999999779
      }, {"x": 361085.571, "y": 6676358.362, "z": 44.296000000002095}, {
        "x": 361098.188,
        "y": 6676374.703,
        "z": 43.97299999999814
      }, {"x": 361110.603, "y": 6676393.101, "z": 43.24700000000303}, {
        "x": 361122.468,
        "y": 6676412.148,
        "z": 41.46799999999348
      }, {"x": 361144.124, "y": 6676447.514, "z": 37.013999999995576}, {
        "x": 361165.84,
        "y": 6676482.876,
        "z": 32.58199999999488
      }, {"x": 361177.695, "y": 6676501.801, "z": 30.81600000000617}, {
        "x": 361180.51,
        "y": 6676505.886,
        "z": 30.52400000000489
      }],
      "id": 454656,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7284,
      "endMValue": 290.777,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904825,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718541,
      "startAddressM": 7284,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 69068,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 361180.51, "y": 6676505.886, "z": 30.52400000000489}, {
        "x": 361190.132,
        "y": 6676518.579,
        "z": 29.904999999998836
      }, {"x": 361203.001, "y": 6676533.543, "z": 29.652000000001863}, {
        "x": 361219.173,
        "y": 6676549.042,
        "z": 29.480999999999767
      }, {"x": 361237.235, "y": 6676564.719, "z": 29.756999999997788}, {
        "x": 361254.768,
        "y": 6676577.014,
        "z": 29.99800000000687
      }, {"x": 361273.485, "y": 6676586.691, "z": 30.927999999999884}, {
        "x": 361292.587,
        "y": 6676594.355,
        "z": 31.762000000002445
      }, {"x": 361314.834, "y": 6676600.527, "z": 32.33400000000256}, {
        "x": 361335.713,
        "y": 6676601.232,
        "z": 32.05000000000291
      }, {"x": 361354.784, "y": 6676598.104, "z": 31.35000000000582}, {
        "x": 361374.414,
        "y": 6676593.11,
        "z": 30.76600000000326
      }, {"x": 361395.075, "y": 6676587.433, "z": 30.24800000000687}, {
        "x": 361406.224,
        "y": 6676584.69,
        "z": 30.06699999999546
      }, {"x": 361421.719, "y": 6676582.606, "z": 29.760999999998603}, {
        "x": 361431.059,
        "y": 6676584.636,
        "z": 29.698000000003958
      }],
      "id": 69068,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7571,
      "endMValue": 286.708,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904831,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897157,
      "startAddressM": 6191,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 64205,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 360420.081, "y": 6675843.041, "z": 41.08299999999872}, {
        "x": 360440.37,
        "y": 6675847.529,
        "z": 42.288000000000466
      }, {"x": 360464.863, "y": 6675853.816, "z": 43.5}, {
        "x": 360490.939,
        "y": 6675859.824,
        "z": 43.62399999999616
      }, {"x": 360511.677, "y": 6675864.145, "z": 43.44999999999709}, {
        "x": 360534.894,
        "y": 6675868.826,
        "z": 43.03100000000268
      }, {"x": 360561.815, "y": 6675875.283, "z": 42.7390000000014}, {
        "x": 360582.26,
        "y": 6675880.276,
        "z": 42.562000000005355
      }, {"x": 360598.898, "y": 6675884.649, "z": 42.75199999999313}, {
        "x": 360615.811,
        "y": 6675888.023,
        "z": 43.05800000000454
      }, {"x": 360620.145, "y": 6675888.939, "z": 43.111999999993714}, {
        "x": 360639.855,
        "y": 6675894.647,
        "z": 43.453999999997905
      }, {"x": 360658.927, "y": 6675899.241, "z": 43.81699999999546}, {
        "x": 360680.533,
        "y": 6675904.397,
        "z": 44.46700000000419
      }, {"x": 360704.647, "y": 6675909.455, "z": 45.21099999999569}, {
        "x": 360714.645,
        "y": 6675911.229,
        "z": 45.49700000000303
      }],
      "id": 64205,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6494,
      "endMValue": 302.446,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904381,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718613,
      "startAddressM": 6929,
      "roadNameFi": "Österbyntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 12458,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11273,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 360960.337, "y": 6676242.382, "z": 50.69899999999325}, {
        "x": 360962.296,
        "y": 6676250.631,
        "z": 51.00900000000547
      }, {"x": 360966.313, "y": 6676265.347, "z": 51.471000000005006}, {
        "x": 360972.422,
        "y": 6676280.684,
        "z": 51.445000000006985
      }, {"x": 360983.027, "y": 6676295.077, "z": 50.671000000002095}, {
        "x": 360987.833,
        "y": 6676299.708,
        "z": 50.221000000005006
      }],
      "id": 12458,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6994,
      "endMValue": 64.794,
      "roadNameSe": "Österbyvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904651,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718518,
      "startAddressM": 4543,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 47691,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362900.544, "y": 6676393.214, "z": 15.638999999995576}, {
        "x": 362884.289,
        "y": 6676402.036,
        "z": 16.028000000005704
      }, {"x": 362851.859, "y": 6676420.308, "z": 16.68399999999383}, {
        "x": 362822.569,
        "y": 6676439.008,
        "z": 17.220000000001164
      }, {"x": 362801.482, "y": 6676451.338, "z": 17.65799999999581}],
      "id": 47691,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4658,
      "endMValue": 114.896,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903476,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718540,
      "startAddressM": 6176,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 54829,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 361422.157, "y": 6676870.831, "z": 24.169999999998254}, {
        "x": 361410.909,
        "y": 6676877.664,
        "z": 25.118000000002212
      }, {"x": 361384.509, "y": 6676888.664, "z": 26.595000000001164}, {
        "x": 361354.453,
        "y": 6676896.319,
        "z": 27.539000000004307
      }, {"x": 361294.443, "y": 6676908.52, "z": 29.137000000002445}, {
        "x": 361261.71,
        "y": 6676915.81,
        "z": 29.81500000000233
      }, {"x": 361231.631, "y": 6676927.672, "z": 30.263999999995576}, {
        "x": 361199.541,
        "y": 6676941.071,
        "z": 30.8579999999929
      }, {"x": 361173.346, "y": 6676952.537, "z": 31.49099999999453}, {
        "x": 361146.293,
        "y": 6676965.909,
        "z": 32.62399999999616
      }, {"x": 361141.209, "y": 6676968.993, "z": 32.937000000005355}],
      "id": 54829,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6476,
      "endMValue": 299.376,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904459,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897896,
      "startAddressM": 5182,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 5214,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362335.798, "y": 6676682.759, "z": 19.380999999993946}, {
        "x": 362306.062,
        "y": 6676697.597,
        "z": 18.62600000000384
      }, {"x": 362273.431, "y": 6676707.676, "z": 18.418999999994412}, {
        "x": 362252.1924356275,
        "y": 6676713.681876812,
        "z": 18.309002256174928
      }],
      "id": 5214,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5271,
      "endMValue": 89.456,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1729703628,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 6552113,
      "startAddressM": 6476,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 65042,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 361141.209, "y": 6676968.993, "z": 32.937000000005355}, {
        "x": 361128.223,
        "y": 6676977.965,
        "z": 33.86999999999534
      }, {"x": 361110.529, "y": 6676992.579, "z": 35.494000000006054}, {
        "x": 361094.273,
        "y": 6677011.674,
        "z": 37.388999999995576
      }, {"x": 361082.535, "y": 6677031.189, "z": 39.11000000000058}, {
        "x": 361071.159,
        "y": 6677052.963,
        "z": 40.70100000000093
      }, {"x": 361062.28, "y": 6677067.588, "z": 41.479999999995925}, {
        "x": 361054.76,
        "y": 6677078.029,
        "z": 41.888999999995576
      }, {"x": 361053.634, "y": 6677079.397, "z": 41.927999999999884}, {
        "x": 361036.965,
        "y": 6677097.396,
        "z": 42.6140000000014
      }, {"x": 361022.529, "y": 6677111.253, "z": 43.20900000000256}, {
        "x": 361000.617,
        "y": 6677131.417,
        "z": 44.24099999999453
      }, {"x": 360979.53, "y": 6677151.133, "z": 45.262000000002445}, {
        "x": 360962.11,
        "y": 6677167.307,
        "z": 46.05100000000675
      }, {"x": 360947.048, "y": 6677182.143, "z": 46.38499999999476}, {
        "x": 360925.817,
        "y": 6677201.079,
        "z": 45.729000000006636
      }, {"x": 360904.508, "y": 6677221.277, "z": 44.00299999999697}, {
        "x": 360899.80335064797,
        "y": 6677225.93365293,
        "z": 43.52003599638054
      }],
      "id": 65042,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6832,
      "endMValue": 355.428,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904843,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718582,
      "startAddressM": 5271,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 29914,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362252.192, "y": 6676713.682, "z": 18.30899999999383}, {
        "x": 362242.318,
        "y": 6676716.258,
        "z": 18.221999999994296
      }, {"x": 362208.831, "y": 6676728.17, "z": 17.820999999996275}, {
        "x": 362173.192,
        "y": 6676741.335,
        "z": 17.710000000006403
      }, {"x": 362134.539, "y": 6676755.322, "z": 18.038000000000466}, {
        "x": 362121.6191907934,
        "y": 6676759.553937504,
        "z": 18.24299697270747
      }],
      "id": 29914,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5410,
      "endMValue": 138.441,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903560,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897890,
      "startAddressM": 5801,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 19741,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 361751.159, "y": 6676702.131, "z": 30.887000000002445}, {
        "x": 361722.344,
        "y": 6676707.642,
        "z": 30.86699999999837
      }, {"x": 361677.403, "y": 6676722.283, "z": 29.1420000000071}, {
        "x": 361631.777,
        "y": 6676739.23,
        "z": 26.903999999994994
      }, {"x": 361591.354, "y": 6676753.727, "z": 25.581999999994878}, {
        "x": 361574.21,
        "y": 6676760.107,
        "z": 25.25800000000163
      }, {"x": 361555.693, "y": 6676769.992, "z": 24.74000000000524}, {
        "x": 361545.51512573357,
        "y": 6676776.4529201845,
        "z": 24.410004076646704
      }],
      "id": 19741,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6021,
      "endMValue": 219.557,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903566,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897894,
      "startAddressM": 6021,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 45393,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 361545.515, "y": 6676776.453, "z": 24.410000000003492}, {
        "x": 361533.581,
        "y": 6676784.773,
        "z": 23.902000000001863
      }, {"x": 361504.029, "y": 6676809.811, "z": 22.581000000005588}, {
        "x": 361464.132,
        "y": 6676841.013,
        "z": 21.631999999997788
      }, {"x": 361430.864, "y": 6676864.969, "z": 23.460000000006403}, {
        "x": 361422.157,
        "y": 6676870.831,
        "z": 24.169999999998254
      }],
      "id": 45393,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6176,
      "endMValue": 155.422,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903584,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717331,
      "startAddressM": 8371,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 66335,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360101.351, "y": 6678431.566, "z": 52.26900000000023}, {
        "x": 360096.883,
        "y": 6678442.045,
        "z": 51.85700000000361
      }, {"x": 360084.417, "y": 6678472.254, "z": 50.703999999997905}, {
        "x": 360072.618,
        "y": 6678501.233,
        "z": 49.81500000000233
      }, {"x": 360060.066, "y": 6678530.846, "z": 49.46400000000722}, {
        "x": 360049.328,
        "y": 6678549.935,
        "z": 49.570000000006985
      }, {"x": 360046.623, "y": 6678553.843, "z": 49.62699999999313}],
      "id": 66335,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8506,
      "endMValue": 134.179,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905581,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897037,
      "startAddressM": 8612,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 68834,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359974.325, "y": 6678631.737, "z": 50.4829999999929}, {
        "x": 359969.808,
        "y": 6678639.991,
        "z": 50.54799999999523
      }, {"x": 359963.327, "y": 6678650.752, "z": 50.71899999999732}, {
        "x": 359950.987,
        "y": 6678669.951,
        "z": 50.98500000000058
      }, {"x": 359942.584, "y": 6678680.884, "z": 50.880999999993946}, {
        "x": 359932.294,
        "y": 6678691.776,
        "z": 50.32300000000396
      }, {"x": 359920.215, "y": 6678702.996, "z": 49.43099999999686}, {
        "x": 359907.058,
        "y": 6678715.469,
        "z": 48.6359999999986
      }, {"x": 359892.929, "y": 6678729.008, "z": 48.23500000000058}, {
        "x": 359890.39828491607,
        "y": 6678731.619705966,
        "z": 48.21300247655543
      }],
      "id": 68834,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8744,
      "endMValue": 131.388,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905617,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499894851,
      "startAddressM": 3619,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 57894,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 363813.336, "y": 6676301.8, "z": 16.53299999999581}, {
        "x": 363782.375,
        "y": 6676302.718,
        "z": 15.569000000003143
      }, {"x": 363736.427, "y": 6676304.694, "z": 14.331999999994878}, {
        "x": 363702.546,
        "y": 6676305.209,
        "z": 14.25800000000163
      }],
      "id": 57894,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3730,
      "endMValue": 110.85,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362926953,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897009,
      "startAddressM": 8785,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 46903,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359862.317, "y": 6678761.065, "z": 48.67600000000675}, {
        "x": 359851.4,
        "y": 6678772.399,
        "z": 48.96499999999651
      }, {"x": 359838.679, "y": 6678785.207, "z": 49.30599999999686}, {
        "x": 359827.525,
        "y": 6678796.504,
        "z": 49.40799999999581
      }, {"x": 359825.901, "y": 6678798.078, "z": 49.388999999995576}, {
        "x": 359815.941,
        "y": 6678807.092,
        "z": 48.97599999999511
      }, {"x": 359797.72, "y": 6678824.257, "z": 47.3579999999929}, {
        "x": 359786.599,
        "y": 6678835.711,
        "z": 46.40899999999965
      }, {"x": 359777.234, "y": 6678847.95, "z": 45.729999999995925}, {
        "x": 359764.067,
        "y": 6678870.285,
        "z": 45.04200000000128
      }, {"x": 359761.518, "y": 6678875.089, "z": 45.00699999999779}, {
        "x": 359749.965,
        "y": 6678896.859,
        "z": 44.961999999999534
      }, {"x": 359737.133, "y": 6678922.351, "z": 45.24199999999837}, {
        "x": 359731.959,
        "y": 6678932.11,
        "z": 45.388000000006286
      }],
      "id": 46903,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 9002,
      "endMValue": 217.364,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905551,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897012,
      "startAddressM": 9108,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 3675,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359683.035, "y": 6679025.157, "z": 47.52400000000489}, {
        "x": 359676.869,
        "y": 6679034.389,
        "z": 47.75500000000466
      }, {"x": 359663.948, "y": 6679050.672, "z": 48.16400000000431}, {
        "x": 359659.637,
        "y": 6679056.378,
        "z": 48.3179999999993
      }, {"x": 359651.654, "y": 6679066.442, "z": 48.64800000000105}, {
        "x": 359627.088,
        "y": 6679094.739,
        "z": 49.830000000001746
      }, {"x": 359621.201, "y": 6679101.626, "z": 50.21700000000419}, {
        "x": 359615.909,
        "y": 6679107.938,
        "z": 50.586999999999534
      }, {"x": 359611.09, "y": 6679114.884, "z": 50.921000000002095}],
      "id": 3675,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 9223,
      "endMValue": 115.11,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905533,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718522,
      "startAddressM": 3986,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 60116,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 363446.931, "y": 6676313.198, "z": 17.105999999999767}, {
        "x": 363445.433,
        "y": 6676313.245,
        "z": 17.09299999999348
      }, {"x": 363407.856, "y": 6676314.761, "z": 17.243000000002212}, {
        "x": 363360.381,
        "y": 6676317.344,
        "z": 17.664000000004307
      }, {"x": 363286.276, "y": 6676322.06, "z": 17.680999999996857}, {
        "x": 363251.265,
        "y": 6676324.46,
        "z": 17.479999999995925
      }, {"x": 363202.335, "y": 6676327.838, "z": 17.37699999999313}, {
        "x": 363192.7272742303,
        "y": 6676328.672976168,
        "z": 17.3530006850076
      }],
      "id": 60116,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4241,
      "endMValue": 254.69,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903464,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717497,
      "startAddressM": 7627,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 5686,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360578.194, "y": 6677943.997, "z": 44.788000000000466}, {
        "x": 360577.046,
        "y": 6677945.304,
        "z": 44.836999999999534
      }, {"x": 360565.457, "y": 6677957.029, "z": 45.369000000006054}, {
        "x": 360550.594,
        "y": 6677968.78,
        "z": 45.48200000000361
      }, {"x": 360535.801, "y": 6677979.013, "z": 44.43300000000454}, {
        "x": 360522.764,
        "y": 6677992.317,
        "z": 43.104999999995925
      }, {"x": 360514.589, "y": 6678009.668, "z": 41.49800000000687}, {
        "x": 360508.104,
        "y": 6678033.967,
        "z": 39.75900000000547
      }, {"x": 360503.002, "y": 6678059.646, "z": 38.49300000000221}, {
        "x": 360499.814,
        "y": 6678078.94,
        "z": 37.78399999999965
      }, {"x": 360496.312, "y": 6678099.228, "z": 37.153000000005704}, {
        "x": 360490.108,
        "y": 6678121.238,
        "z": 36.52400000000489
      }, {"x": 360480.024, "y": 6678134.769, "z": 36.43300000000454}, {
        "x": 360464.013,
        "y": 6678146.236,
        "z": 37.361999999993714
      }, {"x": 360450.856, "y": 6678151.097, "z": 38.302999999999884}, {
        "x": 360439.24,
        "y": 6678153.626,
        "z": 39.161999999996624
      }, {"x": 360427.476, "y": 6678155.682, "z": 39.84200000000419}],
      "id": 5686,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7909,
      "endMValue": 281.735,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905107,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717341,
      "startAddressM": 8506,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 32648,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360046.623, "y": 6678553.843, "z": 49.62699999999313}, {
        "x": 360040.173,
        "y": 6678562.002,
        "z": 49.79399999999441
      }],
      "id": 32648,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8516,
      "endMValue": 10.401,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905587,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717472,
      "startAddressM": 7068,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 1598,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360793.415, "y": 6677433.747, "z": 44.145000000004075}, {
        "x": 360789.87,
        "y": 6677446.254,
        "z": 44.56100000000151
      }, {"x": 360777.643, "y": 6677481.827, "z": 46.15799999999581}, {
        "x": 360764.92,
        "y": 6677518.736,
        "z": 47.070999999996275
      }, {"x": 360759.199, "y": 6677534.814, "z": 47.31100000000151}, {
        "x": 360749.718,
        "y": 6677560.57,
        "z": 47.75100000000384
      }, {"x": 360740.332, "y": 6677588.483, "z": 48.23099999999977}, {
        "x": 360729.81,
        "y": 6677617.827,
        "z": 48.804999999993015
      }, {"x": 360720.474, "y": 6677645.498, "z": 49.562999999994645}, {
        "x": 360710.444,
        "y": 6677676.501,
        "z": 50.06699999999546
      }, {"x": 360701.173, "y": 6677704.286, "z": 49.66800000000512}, {
        "x": 360693.784,
        "y": 6677727.836,
        "z": 49.14400000000023
      }],
      "id": 1598,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7379,
      "endMValue": 310.551,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905053,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718562,
      "startAddressM": 5410,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 60095,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 361824.308, "y": 6676695.831, "z": 30.006999999997788}, {
        "x": 361835.945,
        "y": 6676695.046,
        "z": 29.353000000002794
      }, {"x": 361868.582, "y": 6676694.21, "z": 27.27400000000489}, {
        "x": 361902.333,
        "y": 6676695.633,
        "z": 25.661999999996624
      }, {"x": 361932.072, "y": 6676700.609, "z": 25.104999999995925}, {
        "x": 361955.841,
        "y": 6676708.435,
        "z": 25.12300000000687
      }, {"x": 361980.189, "y": 6676722.974, "z": 25.09900000000198}, {
        "x": 362001.68,
        "y": 6676740.247,
        "z": 24.61599999999453
      }, {"x": 362024.557, "y": 6676756.375, "z": 23.288000000000466}, {
        "x": 362050.472,
        "y": 6676765.87,
        "z": 21.47599999999511
      }, {"x": 362076.307, "y": 6676767.376, "z": 19.8179999999993}, {
        "x": 362104.109,
        "y": 6676764.375,
        "z": 18.627999999996973
      }, {"x": 362121.619, "y": 6676759.554, "z": 18.243000000002212}],
      "id": 60095,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5727,
      "endMValue": 316.794,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906635,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718539,
      "startAddressM": 4341,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 36419,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 363093.537, "y": 6676338.243, "z": 17.206000000005588}, {
        "x": 363077.746,
        "y": 6676340.612,
        "z": 17.114000000001397
      }, {"x": 363056.187, "y": 6676344.476, "z": 16.926999999996042}, {
        "x": 363020.913,
        "y": 6676350.967,
        "z": 16.389999999999418
      }, {"x": 362984.6, "y": 6676360.451, "z": 15.520999999993364}, {
        "x": 362949.856,
        "y": 6676372.369,
        "z": 15.198999999993248
      }, {"x": 362916.934, "y": 6676385.047, "z": 15.39100000000326}, {
        "x": 362900.544,
        "y": 6676393.214,
        "z": 15.638999999995576
      }],
      "id": 36419,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4543,
      "endMValue": 201.59,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362907073,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 151517,
      "startAddressM": 3730,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 21861,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 363702.546, "y": 6676305.209, "z": 14.25800000000163}, {
        "x": 363530.44,
        "y": 6676310.587,
        "z": 16.236000000004424
      }, {"x": 363446.931, "y": 6676313.198, "z": 17.105999999999767}],
      "id": 21861,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3986,
      "endMValue": 255.74,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362926623,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717330,
      "startAddressM": 8096,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 30459,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360256.083, "y": 6678207.702, "z": 44.921000000002095}, {
        "x": 360249.497,
        "y": 6678215.297,
        "z": 44.99300000000221
      }, {"x": 360246.7670103255, "y": 6678218.560987654, "z": 44.98700002268716}],
      "id": 30459,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8111,
      "endMValue": 14.308,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906791,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717283,
      "startAddressM": 9002,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 13961,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359731.959, "y": 6678932.11, "z": 45.388000000006286}, {
        "x": 359725.24,
        "y": 6678945.576,
        "z": 45.619000000006054
      }, {"x": 359714.676, "y": 6678967.593, "z": 46.12699999999313}, {
        "x": 359705.866,
        "y": 6678985.158,
        "z": 46.544999999998254
      }, {"x": 359694.619, "y": 6679006.395, "z": 47.08299999999872}, {
        "x": 359686.281,
        "y": 6679019.886,
        "z": 47.39999999999418
      }, {"x": 359683.0351573948, "y": 6679025.156744415, "z": 47.52399398738572}],
      "id": 13961,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 9108,
      "endMValue": 105.201,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905665,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717346,
      "startAddressM": 8319,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 49326,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360119.709, "y": 6678382.981, "z": 55.88000000000466}, {
        "x": 360115.77,
        "y": 6678394.204,
        "z": 54.97299999999814
      }, {"x": 360108.669, "y": 6678414.088, "z": 53.2609999999986}, {
        "x": 360101.35110823874,
        "y": 6678431.565741487,
        "z": 52.269014672423886
      }],
      "id": 49326,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8371,
      "endMValue": 51.956,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905131,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718576,
      "startAddressM": 5727,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 28330,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 361824.308, "y": 6676695.831, "z": 30.006999999997788}, {
        "x": 361819.764,
        "y": 6676696.138,
        "z": 30.19199999999546
      }, {"x": 361819.036, "y": 6676696.187, "z": 30.220000000001164}, {
        "x": 361786.119,
        "y": 6676698.535,
        "z": 30.929000000003725
      }, {"x": 361758.374, "y": 6676701.148, "z": 30.910000000003492}, {
        "x": 361751.15907105914,
        "y": 6676702.130990319,
        "z": 30.887000226525025
      }],
      "id": 28330,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5801,
      "endMValue": 73.434,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906803,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717333,
      "startAddressM": 8516,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 21370,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360040.173, "y": 6678562.002, "z": 49.79399999999441}, {
        "x": 360032.971,
        "y": 6678569.74,
        "z": 49.979999999995925
      }, {"x": 360020.428, "y": 6678582.708, "z": 50.229999999995925}, {
        "x": 360002.347,
        "y": 6678599.94,
        "z": 50.33100000000559
      }, {"x": 359986.998, "y": 6678616.261, "z": 50.42200000000594}, {
        "x": 359974.3250997648,
        "y": 6678631.736878169,
        "z": 50.4829995197869
      }],
      "id": 21370,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8612,
      "endMValue": 95.997,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904861,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717504,
      "startAddressM": 7379,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 2244,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360693.784, "y": 6677727.836, "z": 49.14400000000023}, {
        "x": 360690.735,
        "y": 6677735.72,
        "z": 48.93499999999767
      }, {"x": 360681.615, "y": 6677762.55, "z": 48.11800000000221}, {
        "x": 360673.285,
        "y": 6677787.346,
        "z": 46.8579999999929
      }, {"x": 360664.823, "y": 6677806.727, "z": 45.75999999999476}, {
        "x": 360652.094,
        "y": 6677823.605,
        "z": 44.71700000000419
      }, {"x": 360636.514, "y": 6677842.536, "z": 43.88199999999779}, {
        "x": 360623.2,
        "y": 6677861.224,
        "z": 43.48399999999674
      }, {"x": 360610.055, "y": 6677884.62, "z": 43.429000000003725}, {
        "x": 360601.479,
        "y": 6677903.951,
        "z": 43.536999999996624
      }, {"x": 360592.271, "y": 6677923.792, "z": 44.09200000000419}, {
        "x": 360578.1941152441,
        "y": 6677943.996834588,
        "z": 44.78799430206264
      }],
      "id": 2244,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7627,
      "endMValue": 247.182,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905035,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499894820,
      "startAddressM": 2975,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 34169,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 364423.548, "y": 6676150.284, "z": 32.56100000000151}, {
        "x": 364396.71,
        "y": 6676171.701,
        "z": 32.94800000000396
      }, {"x": 364362.462, "y": 6676193.999, "z": 33.41099999999278}, {
        "x": 364328.088,
        "y": 6676212.897,
        "z": 33.513000000006286
      }, {"x": 364294.781, "y": 6676229.059, "z": 33.3579999999929}, {
        "x": 364256.292,
        "y": 6676246.689,
        "z": 33.02300000000105
      }, {"x": 364219.084, "y": 6676259.184, "z": 32.35599999999977}, {
        "x": 364180.024,
        "y": 6676269.716,
        "z": 31.448999999993248
      }, {"x": 364160.685, "y": 6676274.008, "z": 30.861999999993714}, {
        "x": 364117.865,
        "y": 6676281.797,
        "z": 29.50900000000547
      }, {"x": 364077.754, "y": 6676287.523, "z": 27.910999999992782}, {
        "x": 364028.688,
        "y": 6676292.825,
        "z": 25.702000000004773
      }, {"x": 363981.436, "y": 6676296.171, "z": 23.345000000001164}, {
        "x": 363932.378,
        "y": 6676298.235,
        "z": 21.137000000002445
      }, {"x": 363888.763, "y": 6676300.003, "z": 19.202000000004773}, {
        "x": 363849.365,
        "y": 6676301.192,
        "z": 17.75199999999313
      }, {"x": 363813.3364286842, "y": 6676301.799992765, "z": 16.53301450403343}],
      "id": 34169,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3619,
      "endMValue": 642.264,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1729822765,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718519,
      "startAddressM": 4790,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 29461,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362682.514, "y": 6676508.721, "z": 19.801000000006752}, {
        "x": 362666.539,
        "y": 6676512.735,
        "z": 19.85899999999674
      }, {"x": 362638.996, "y": 6676520.211, "z": 19.861999999993714}, {
        "x": 362612.123,
        "y": 6676528.166,
        "z": 19.88300000000163
      }, {"x": 362587.248, "y": 6676537.069, "z": 20.021999999997206}, {
        "x": 362565.702,
        "y": 6676547.547,
        "z": 20.00199999999313
      }, {"x": 362534.092, "y": 6676565.638, "z": 19.98399999999674}, {
        "x": 362504.301,
        "y": 6676584.02,
        "z": 20.218999999997322
      }, {"x": 362471.393, "y": 6676603.124, "z": 20.471999999994296}, {
        "x": 362434.637,
        "y": 6676619.511,
        "z": 20.713000000003376
      }, {"x": 362398.014, "y": 6676638.973, "z": 20.94100000000617}, {
        "x": 362382.4490285991,
        "y": 6676648.969981631,
        "z": 20.895000084524234
      }],
      "id": 29461,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5124,
      "endMValue": 333.109,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903500,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718515,
      "startAddressM": 4658,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 41959,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362801.482, "y": 6676451.338, "z": 17.65799999999581}, {
        "x": 362789.668,
        "y": 6676458.442,
        "z": 17.8920000000071
      }, {"x": 362756.276, "y": 6676478.05, "z": 18.585999999995693}, {
        "x": 362723.129,
        "y": 6676493.539,
        "z": 19.394000000000233
      }, {"x": 362693.392, "y": 6676504.549, "z": 19.735000000000582}, {
        "x": 362682.51443961187,
        "y": 6676508.720831397,
        "z": 19.80099733275327
      }],
      "id": 41959,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4790,
      "endMValue": 132.456,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903488,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897895,
      "startAddressM": 5124,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 63582,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 362382.449, "y": 6676648.97, "z": 20.895000000004075}, {
        "x": 362371.175,
        "y": 6676657.486,
        "z": 20.637000000002445
      }, {"x": 362339.067, "y": 6676680.657, "z": 19.46799999999348}, {
        "x": 362335.7980511622,
        "y": 6676682.758967102,
        "z": 19.3810013616069
      }],
      "id": 63582,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5182,
      "endMValue": 57.611,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362903524,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717329,
      "startAddressM": 8075,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 51077,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360271.06, "y": 6678192.595, "z": 44.796000000002095}, {
        "x": 360265.298,
        "y": 6678198.025,
        "z": 44.763999999995576
      }, {"x": 360256.0830520565, "y": 6678207.701945333, "z": 44.92099911309276}],
      "id": 51077,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8096,
      "endMValue": 21.28,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906785,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718527,
      "startAddressM": 4241,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 18527,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 363192.727, "y": 6676328.673, "z": 17.353000000002794}, {
        "x": 363165.03,
        "y": 6676331.079,
        "z": 17.319000000003143
      }, {"x": 363129.055, "y": 6676334.459, "z": 17.327999999994063}, {
        "x": 363111.282,
        "y": 6676335.975,
        "z": 17.2670000000071
      }, {"x": 363093.537, "y": 6676338.243, "z": 17.206000000005588}],
      "id": 18527,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4341,
      "endMValue": 99.662,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362907079,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717268,
      "startAddressM": 8111,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 67103,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360246.767, "y": 6678218.561, "z": 44.986999999993714}, {
        "x": 360232.814,
        "y": 6678235.728,
        "z": 45.137000000002445
      }, {"x": 360200.051, "y": 6678276.568, "z": 46.71700000000419}, {
        "x": 360183.999,
        "y": 6678296.314,
        "z": 48.721999999994296
      }, {"x": 360170.456, "y": 6678313.068, "z": 51.2390000000014}, {
        "x": 360156.666,
        "y": 6678329.652,
        "z": 53.75100000000384
      }, {"x": 360144.814, "y": 6678343.928, "z": 55.33999999999651}, {
        "x": 360131.521,
        "y": 6678360.909,
        "z": 56.46400000000722
      }, {"x": 360122.857, "y": 6678375.341, "z": 56.35099999999511}, {
        "x": 360119.709,
        "y": 6678382.981,
        "z": 55.88000000000466
      }],
      "id": 67103,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8319,
      "endMValue": 208.255,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362904891,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717339,
      "startAddressM": 8744,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 55918,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359890.398, "y": 6678731.62, "z": 48.213000000003376}, {
        "x": 359878.522,
        "y": 6678744.239,
        "z": 48.25900000000547
      }, {"x": 359862.3170821226, "y": 6678761.06491473, "z": 48.675997886761806}],
      "id": 55918,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8785,
      "endMValue": 40.689,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905959,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718544,
      "startAddressM": 6832,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 41870,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360899.803, "y": 6677225.934, "z": 43.520000000004075}, {
        "x": 360883.971,
        "y": 6677243.07,
        "z": 41.862999999997555
      }, {"x": 360872.677, "y": 6677257.481, "z": 40.887000000002445}, {
        "x": 360854.947,
        "y": 6677284.407,
        "z": 40.36699999999837
      }, {"x": 360839.313, "y": 6677313.008, "z": 41.130999999993946}, {
        "x": 360826.62,
        "y": 6677338.111,
        "z": 42.520999999993364
      }, {"x": 360816.618, "y": 6677363.021, "z": 43.64800000000105}, {
        "x": 360806.274,
        "y": 6677396.562,
        "z": 43.845000000001164
      }, {"x": 360797.402, "y": 6677421.27, "z": 43.88400000000547}, {
        "x": 360793.4150603475,
        "y": 6677433.746811148,
        "z": 44.14499604948839
      }],
      "id": 41870,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7068,
      "endMValue": 235.897,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905047,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717270,
      "startAddressM": 9223,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 18007,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 359611.09, "y": 6679114.884, "z": 50.921000000002095}, {
        "x": 359599.844,
        "y": 6679129.241,
        "z": 51.67299999999523
      }, {"x": 359585.404, "y": 6679148.798, "z": 52.736999999993714}, {
        "x": 359572.461,
        "y": 6679170.686,
        "z": 53.74199999999837
      }, {"x": 359561.772, "y": 6679191.982, "z": 54.54200000000128}, {
        "x": 359555.613,
        "y": 6679206.958,
        "z": 54.98399999999674
      }],
      "id": 18007,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 9331,
      "endMValue": 107.997,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 359555.6152838653, "y": 6679206.952446636, "z": 54.983836098638974},
        "value": 9331
      }],
      "mmlId": 362905689,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717325,
      "startAddressM": 7909,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 46812,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 360427.476, "y": 6678155.682, "z": 39.84200000000419}, {
        "x": 360407.959,
        "y": 6678156.682,
        "z": 40.36500000000524
      }, {"x": 360386.197, "y": 6678157.35, "z": 40.45299999999406}, {
        "x": 360363.155,
        "y": 6678157.594,
        "z": 41.088000000003376
      }, {"x": 360339.26, "y": 6678158.896, "z": 42.44000000000233}, {
        "x": 360321.017,
        "y": 6678161.75,
        "z": 43.41099999999278
      }, {"x": 360300.237, "y": 6678171.018, "z": 44.278000000005704}, {
        "x": 360277.939,
        "y": 6678186.657,
        "z": 44.69999999999709
      }, {"x": 360271.06, "y": 6678192.595, "z": 44.796000000002095}],
      "id": 46812,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8075,
      "endMValue": 165.83,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905113,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718385,
      "startAddressM": 1267,
      "roadNameFi": "Turuntie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 10746,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 361866.084, "y": 6682449.317, "z": 58.470000000001164}, {
        "x": 361850.625,
        "y": 6682465.332,
        "z": 59.086999999999534
      }, {"x": 361822.67, "y": 6682494.405, "z": 59.33299999999872}, {
        "x": 361778.776,
        "y": 6682540.43,
        "z": 57.52599999999802
      }, {"x": 361752.687, "y": 6682567.98, "z": 55.25900000000547}, {
        "x": 361728.053,
        "y": 6682594.252,
        "z": 53.13499999999476
      }, {"x": 361704.631, "y": 6682619.017, "z": 51.661999999996624}, {
        "x": 361683.276,
        "y": 6682640.593,
        "z": 51.06100000000151
      }, {"x": 361670.28, "y": 6682651.136, "z": 51.520000000004075}, {
        "x": 361646.955,
        "y": 6682668.159,
        "z": 52.83100000000559
      }],
      "id": 10746,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1577,
      "endMValue": 310.204,
      "roadNameSe": "Åbovägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083197,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150570,
      "startAddressM": 446,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 45959,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 3,
      "points": [{"x": 362308.032, "y": 6682119.417, "z": 59.479999999995925}, {
        "x": 362325.285,
        "y": 6682114.904,
        "z": 59.737999999997555
      }, {"x": 362369.873, "y": 6682106.286, "z": 60.8350000000064}, {
        "x": 362420.03,
        "y": 6682104.657,
        "z": 62.51600000000326
      }, {"x": 362462.198, "y": 6682109.194, "z": 64.39500000000407}, {
        "x": 362497.648,
        "y": 6682115.908,
        "z": 66.39400000000023
      }, {"x": 362531.452, "y": 6682123.797, "z": 68.00699999999779}, {
        "x": 362561.4637185178,
        "y": 6682129.909942666,
        "z": 68.53699502913274
      }],
      "id": 45959,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 703,
      "endMValue": 257.262,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082759,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718382,
      "startAddressM": 1121,
      "roadNameFi": "Turuntie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 34437,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 361966.115, "y": 6682342.526, "z": 52.005999999993946}, {
        "x": 361945.795,
        "y": 6682366.356,
        "z": 52.37799999999697
      }, {"x": 361919.922, "y": 6682392.895, "z": 54.06699999999546}, {
        "x": 361904.031,
        "y": 6682409.431,
        "z": 55.4429999999993
      }, {"x": 361879.277, "y": 6682435.649, "z": 57.612999999997555}, {
        "x": 361866.0840715349,
        "y": 6682449.316925889,
        "z": 58.46999535318405
      }],
      "id": 34437,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1267,
      "endMValue": 146.369,
      "roadNameSe": "Åbovägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082879,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150572,
      "startAddressM": 703,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 996,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362308.032, "y": 6682119.417, "z": 59.479999999995925}, {
        "x": 362282.243,
        "y": 6682126.795,
        "z": 58.96700000000419
      }, {"x": 362253.839, "y": 6682135.081, "z": 58.49099999999453}, {
        "x": 362252.21705210855,
        "y": 6682135.584983808,
        "z": 58.463000899534705
      }],
      "id": 996,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 761,
      "endMValue": 58.11,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1084848088,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718392,
      "startAddressM": 761,
      "roadNameFi": "Turuntie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 15677,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362252.217, "y": 6682135.585, "z": 58.463000000003376}, {
        "x": 362222.2583038428,
        "y": 6682144.411910476,
        "z": 57.84300628800574
      }],
      "id": 15677,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 793,
      "endMValue": 31.232,
      "roadNameSe": "Åbovägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 361978030,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718391,
      "startAddressM": 793,
      "roadNameFi": "Turuntie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 33929,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362222.258, "y": 6682144.412, "z": 57.84299999999348}, {
        "x": 362196.348,
        "y": 6682154.374,
        "z": 56.552999999999884
      }, {"x": 362160.33, "y": 6682170.971, "z": 54.53900000000431}, {
        "x": 362124.564,
        "y": 6682192.622,
        "z": 52.36699999999837
      }, {"x": 362097.719, "y": 6682212.658, "z": 51.67699999999604}, {
        "x": 362070.429,
        "y": 6682236.795,
        "z": 51.88499999999476
      }, {"x": 362049.821, "y": 6682257.347, "z": 52.16099999999278}, {
        "x": 362021.276,
        "y": 6682286.493,
        "z": 52.544999999998254
      }, {"x": 361994.499, "y": 6682315.126, "z": 52.39400000000023}, {
        "x": 361967.94,
        "y": 6682341.136,
        "z": 52.021999999997206
      }, {"x": 361966.1153550048, "y": 6682342.525729612, "z": 52.00600311236465}],
      "id": 33929,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1121,
      "endMValue": 327.727,
      "roadNameSe": "Åbovägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082867,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150421,
      "startAddressM": 344,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 59911,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 3,
      "points": [{"x": 362561.464, "y": 6682129.91, "z": 68.53699999999662}, {
        "x": 362587.27,
        "y": 6682136.638,
        "z": 68.50900000000547
      }, {"x": 362616.181, "y": 6682143.057, "z": 68.56100000000151}, {
        "x": 362640.088,
        "y": 6682147.126,
        "z": 68.47599999999511
      }, {"x": 362661.057, "y": 6682149.008, "z": 68.36699999999837}],
      "id": 59911,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 446,
      "endMValue": 101.588,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082771,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150425,
      "startAddressM": 261,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 54929,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362743.783, "y": 6682147.029, "z": 67.10899999999674}, {
        "x": 362733.48436762334,
        "y": 6682148.234956952,
        "z": 67.36599082636658
      }],
      "id": 54929,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 272,
      "endMValue": 10.369,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082789,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150427,
      "startAddressM": 0,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 18680,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362981.914, "y": 6682045.684, "z": 63.270999999993364}, {
        "x": 362963.687,
        "y": 6682056.68,
        "z": 63.17399999999907
      }, {"x": 362940.561, "y": 6682070.466, "z": 62.77499999999418}, {
        "x": 362907.237,
        "y": 6682089.812,
        "z": 62.87399999999616
      }, {"x": 362880.288, "y": 6682105.046, "z": 63.58999999999651}, {
        "x": 362848.818,
        "y": 6682119.726,
        "z": 64.49899999999616
      }, {"x": 362818.521, "y": 6682130.941, "z": 65.3350000000064}, {
        "x": 362784.143,
        "y": 6682140.05,
        "z": 66.15899999999965
      }, {"x": 362746.214, "y": 6682146.744, "z": 67.02999999999884}, {
        "x": 362743.78343619447,
        "y": 6682147.028948862,
        "z": 67.10898582502212
      }],
      "id": 18680,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 261,
      "endMValue": 261.258,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 362981.914, "y": 6682045.684, "z": 63.270999999993364}, "value": 0}],
      "mmlId": 356082801,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150422,
      "startAddressM": 272,
      "roadNameFi": "Nupurintie",
      "roadPartNumber": 9,
      "administrativeClassMML": "State",
      "segmentId": 28439,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 110,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 362733.484, "y": 6682148.235, "z": 67.36599999999453}, {
        "x": 362712.595,
        "y": 6682149.788,
        "z": 67.91300000000047
      }, {"x": 362686.133, "y": 6682150.446, "z": 68.3070000000007}, {
        "x": 362661.0570268616,
        "y": 6682149.008001541,
        "z": 68.36699993572596
      }],
      "id": 28439,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 344,
      "endMValue": 72.534,
      "roadNameSe": "Nupurbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082783,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717138,
      "startAddressM": 1627,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454832,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358015.136, "y": 6678679.667, "z": 47.812000000005355}, {
        "x": 358037.587,
        "y": 6678703.008,
        "z": 48.770999999993364
      }, {"x": 358060.716, "y": 6678728.725, "z": 49.7609999999986}, {
        "x": 358061.84,
        "y": 6678729.908,
        "z": 49.794999999998254
      }],
      "id": 454832,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1696,
      "endMValue": 68.60556052297123,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889158,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897073,
      "startAddressM": 526,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454817,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358951.67, "y": 6679007.532, "z": 52.177999999999884}, {
        "x": 358953.97,
        "y": 6679007.973,
        "z": 52.16400000000431
      }, {"x": 358983.151, "y": 6679013.837, "z": 52.12200000000303}, {
        "x": 359009.166,
        "y": 6679020.095,
        "z": 51.95600000000559
      }, {"x": 359014.669, "y": 6679021.624, "z": 51.87300000000687}, {
        "x": 359035.961,
        "y": 6679027.58,
        "z": 51.455000000001746
      }, {"x": 359058.868, "y": 6679032.909, "z": 50.7899999999936}, {
        "x": 359069.376,
        "y": 6679035.357,
        "z": 50.49899999999616
      }],
      "id": 454817,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 647,
      "endMValue": 120.99225385709204,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905887,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499897074,
      "startAddressM": 451,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454816,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359069.376, "y": 6679035.357, "z": 50.49899999999616}, {
        "x": 359082.382,
        "y": 6679038.388,
        "z": 50.138000000006286
      }, {"x": 359106.336, "y": 6679043.004, "z": 49.586999999999534}, {
        "x": 359127.451,
        "y": 6679046.073,
        "z": 49.38300000000163
      }, {"x": 359142.908, "y": 6679048.109, "z": 49.3179999999993}],
      "id": 454816,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 526,
      "endMValue": 74.67659975794213,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905863,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717176,
      "startAddressM": 2223,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454840,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357396.054, "y": 6678408.933, "z": 39.013999999995576}, {
        "x": 357405.15,
        "y": 6678411.748,
        "z": 38.76600000000326
      }, {"x": 357432.522, "y": 6678422.687, "z": 38.32399999999325}, {
        "x": 357469.12,
        "y": 6678430.587,
        "z": 37.81500000000233
      }, {"x": 357499.658, "y": 6678436.006, "z": 37.38300000000163}, {
        "x": 357530.779,
        "y": 6678443.996,
        "z": 36.83999999999651
      }, {"x": 357544.591, "y": 6678449.335, "z": 36.60099999999511}],
      "id": 454840,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2378,
      "endMValue": 154.3928386253284,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889722,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717403,
      "startAddressM": 153,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454812,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359328.595, "y": 6679079.192, "z": 51.095000000001164}, {
        "x": 359332.318,
        "y": 6679080.842,
        "z": 51.08199999999488
      }, {"x": 359354.717, "y": 6679091.84, "z": 51.078999999997905}, {
        "x": 359376.362,
        "y": 6679103.414,
        "z": 51.15200000000186
      }, {"x": 359399.431, "y": 6679117.082, "z": 51.50999999999476}, {
        "x": 359423.219,
        "y": 6679130.725,
        "z": 52.095000000001164
      }],
      "id": 454812,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 261,
      "endMValue": 107.80743376538621,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362907895,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717377,
      "startAddressM": 1028,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454825,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358558.025, "y": 6678910.724, "z": 57.52499999999418}, {
        "x": 358560.426,
        "y": 6678912.686,
        "z": 57.538000000000466
      }, {"x": 358582.276, "y": 6678930.977, "z": 57.736999999993714}, {
        "x": 358594.785,
        "y": 6678940.735,
        "z": 57.788000000000466
      }],
      "id": 454825,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1075,
      "endMValue": 47.460858838621306,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888762,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717392,
      "startAddressM": 350,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454815,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359142.908, "y": 6679048.109, "z": 49.3179999999993}, {
        "x": 359154.579,
        "y": 6679049.213,
        "z": 49.38300000000163
      }, {"x": 359175.797, "y": 6679050.654, "z": 49.41599999999744}, {
        "x": 359205.073,
        "y": 6679052.249,
        "z": 49.36999999999534
      }, {"x": 359233.632, "y": 6679055.2, "z": 49.596000000005006}, {
        "x": 359243.445,
        "y": 6679056.642,
        "z": 49.721999999994296
      }],
      "id": 454815,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 451,
      "endMValue": 100.93883397237225,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905905,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717135,
      "startAddressM": 1775,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454834,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357821.514, "y": 6678585.365, "z": 38.48399999999674}, {
        "x": 357832.973,
        "y": 6678589.709,
        "z": 39.12300000000687
      }, {"x": 357863.077, "y": 6678600.92, "z": 40.646999999997206}, {
        "x": 357894.391,
        "y": 6678612.48,
        "z": 42.42600000000675
      }, {"x": 357920.374, "y": 6678622.377, "z": 43.770999999993364}, {
        "x": 357947.056,
        "y": 6678633.069,
        "z": 45.09200000000419
      }, {"x": 357950.142, "y": 6678634.535, "z": 45.2390000000014}],
      "id": 454834,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1913,
      "endMValue": 137.72329578201368,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889380,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717380,
      "startAddressM": 1021,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454824,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358594.785, "y": 6678940.735, "z": 57.788000000000466}, {
        "x": 358599.713,
        "y": 6678945.133,
        "z": 57.78100000000268
      }],
      "id": 454824,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1028,
      "endMValue": 6.605118318435748,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888804,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717275,
      "startAddressM": 0,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454809,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359540.682, "y": 6679199.184, "z": 55.00100000000384}, {
        "x": 359542.395,
        "y": 6679200.065,
        "z": 55.070000000006985
      }, {"x": 359555.613, "y": 6679206.958, "z": 54.98399999999674}],
      "id": 454809,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 17,
      "endMValue": 16.83361988060823,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905203,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717664,
      "startAddressM": 2921,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454844,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357282.476, "y": 6677729.044, "z": 37.86999999999534}, {
        "x": 357283.405,
        "y": 6677756.866,
        "z": 37.30800000000454
      }, {"x": 357285.937, "y": 6677787.442, "z": 36.79099999999744}, {
        "x": 357292.316,
        "y": 6677830.103,
        "z": 36.13999999999942
      }, {"x": 357299.578, "y": 6677876.231, "z": 35.57799999999406}, {
        "x": 357305.278,
        "y": 6677913.296,
        "z": 35.205000000001746
      }, {"x": 357305.863, "y": 6677917.204, "z": 35.14299999999639}],
      "id": 454844,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3111,
      "endMValue": 189.80184687613527,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889590,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717361,
      "startAddressM": 833,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454821,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358671.441, "y": 6678998.552, "z": 55.45699999999488}, {
        "x": 358676.667,
        "y": 6679001.003,
        "z": 55.36599999999453
      }, {"x": 358696.977, "y": 6679008.559, "z": 55.00900000000547}, {
        "x": 358722.84,
        "y": 6679012.903,
        "z": 54.713000000003376
      }, {"x": 358749.109, "y": 6679013.2, "z": 54.5170000000071}, {
        "x": 358766.935,
        "y": 6679011.54,
        "z": 54.229000000006636
      }],
      "id": 454821,
      "administrativeClassId": "1",
      "status": 1,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 931,
      "endMValue": 97.84130343365737,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905785,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717387,
      "startAddressM": 1075,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454826,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358501.075, "y": 6678864.95, "z": 57.11599999999453}, {
        "x": 358517.629,
        "y": 6678877.317,
        "z": 57.25800000000163
      }, {"x": 358540.548, "y": 6678896.641, "z": 57.40899999999965}, {
        "x": 358558.025,
        "y": 6678910.724,
        "z": 57.52499999999418
      }],
      "id": 454826,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1148,
      "endMValue": 73.08668505306132,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889242,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717145,
      "startAddressM": 1931,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454836,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357708.537, "y": 6678541.372, "z": 33.59399999999732}, {
        "x": 357719.739,
        "y": 6678546.658,
        "z": 33.81900000000314
      }, {"x": 357747.583, "y": 6678557.879, "z": 34.838000000003376}, {
        "x": 357776.451,
        "y": 6678568.267,
        "z": 36.06100000000151
      }, {"x": 357801.275, "y": 6678577.694, "z": 37.34100000000035}, {
        "x": 357804.745,
        "y": 6678579.009,
        "z": 37.52599999999802
      }],
      "id": 454836,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2035,
      "endMValue": 103.35120267389195,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1003607750,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717374,
      "startAddressM": 959,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454823,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358599.713, "y": 6678945.133, "z": 57.78100000000268}, {
        "x": 358601.644,
        "y": 6678946.448,
        "z": 57.771999999997206
      }, {"x": 358621.812, "y": 6678964.766, "z": 57.41000000000349}, {
        "x": 358630.04,
        "y": 6678971.657,
        "z": 57.10099999999511
      }, {"x": 358638.064, "y": 6678977.863, "z": 56.78599999999278}, {
        "x": 358647.408,
        "y": 6678984.55,
        "z": 56.31399999999849
      }],
      "id": 454823,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1021,
      "endMValue": 61.948020518025565,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362888798,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717658,
      "startAddressM": 2554,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454843,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357305.863, "y": 6677917.204, "z": 35.14299999999639}, {
        "x": 357309.362,
        "y": 6677949.145,
        "z": 34.853000000002794
      }, {"x": 357309.606, "y": 6677976.4, "z": 34.55800000000454}, {
        "x": 357306.282,
        "y": 6678004.488,
        "z": 34.52700000000186
      }, {"x": 357296.822, "y": 6678035.872, "z": 34.653000000005704}, {
        "x": 357286.932,
        "y": 6678066.524,
        "z": 34.94999999999709
      }, {"x": 357286.403, "y": 6678068.156, "z": 34.986999999993714}, {
        "x": 357277.402,
        "y": 6678099.59,
        "z": 35.47699999999895
      }, {"x": 357272.976, "y": 6678127.427, "z": 36.00699999999779}, {
        "x": 357271.976,
        "y": 6678157.165,
        "z": 36.71600000000035
      }, {"x": 357272.169, "y": 6678162.802, "z": 36.8179999999993}, {
        "x": 357274.344,
        "y": 6678190.042,
        "z": 37.37200000000303
      }, {"x": 357275.522, "y": 6678199.739, "z": 37.59100000000035}, {
        "x": 357277.849,
        "y": 6678217.463,
        "z": 37.94899999999325
      }, {"x": 357279.407, "y": 6678229.326, "z": 38.24099999999453}, {
        "x": 357285.617,
        "y": 6678264.903,
        "z": 38.945000000006985
      }, {"x": 357288.493, "y": 6678276.228, "z": 39.205000000001746}],
      "id": 454843,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2921,
      "endMValue": 365.38899707174073,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889650,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717656,
      "startAddressM": 3111,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454845,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357295.025, "y": 6677567.037, "z": 39.861999999993714}, {
        "x": 357294.959,
        "y": 6677593.362,
        "z": 40.08400000000256
      }, {"x": 357294.229, "y": 6677613.776, "z": 40.138000000006286}, {
        "x": 357292.428,
        "y": 6677627.466,
        "z": 40.02499999999418
      }, {"x": 357287.953, "y": 6677660.014, "z": 39.421000000002095}, {
        "x": 357283.99,
        "y": 6677689.952,
        "z": 38.720000000001164
      }, {"x": 357282.788, "y": 6677710.699, "z": 38.20699999999488}, {
        "x": 357282.476,
        "y": 6677729.044,
        "z": 37.86999999999534
      }],
      "id": 454845,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3274,
      "endMValue": 162.74288324218415,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889584,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717184,
      "startAddressM": 2378,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454841,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357323.683, "y": 6678349.106, "z": 40.288000000000466}, {
        "x": 357326.22,
        "y": 6678352.416,
        "z": 40.31500000000233
      }, {"x": 357347.657, "y": 6678375.602, "z": 39.93099999999686}, {
        "x": 357372.151,
        "y": 6678395.222,
        "z": 39.44100000000617
      }, {"x": 357386.416, "y": 6678402.604, "z": 39.15700000000652}, {
        "x": 357396.054,
        "y": 6678408.933,
        "z": 39.013999999995576
      }],
      "id": 454841,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2473,
      "endMValue": 94.72316663014428,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889716,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717148,
      "startAddressM": 2035,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454837,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357700.735, "y": 6678537.673, "z": 33.53599999999278}, {
        "x": 357708.537,
        "y": 6678541.372,
        "z": 33.59399999999732
      }],
      "id": 454837,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2043,
      "endMValue": 8.634454528258154,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889512,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717406,
      "startAddressM": 261,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454813,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359254.43, "y": 6679058.272, "z": 49.90700000000652}, {
        "x": 359261.175,
        "y": 6679059.459,
        "z": 49.99199999999837
      }, {"x": 359286.799, "y": 6679065.073, "z": 50.57300000000396}, {
        "x": 359311.489,
        "y": 6679072.472,
        "z": 50.95799999999872
      }, {"x": 359328.595, "y": 6679079.192, "z": 51.095000000001164}],
      "id": 454813,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 339,
      "endMValue": 77.23386889687089,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905899,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717683,
      "startAddressM": 3367,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454847,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357274.218, "y": 6677465.435, "z": 36.828999999997905}, {
        "x": 357278.592,
        "y": 6677475.9,
        "z": 37.15099999999802
      }],
      "id": 454847,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3378,
      "endMValue": 11.342314623412078,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889572,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717146,
      "startAddressM": 1913,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454835,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357804.745, "y": 6678579.009, "z": 37.52599999999802}, {
        "x": 357821.514,
        "y": 6678585.365,
        "z": 38.48399999999674
      }],
      "id": 454835,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1931,
      "endMValue": 17.933156359345954,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889518,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896939,
      "startAddressM": 1446,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454829,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358103.914, "y": 6678766.914, "z": 51.38199999999779}, {
        "x": 358105.597,
        "y": 6678768.138,
        "z": 51.44800000000396
      }, {"x": 358116.65, "y": 6678775.443, "z": 51.80800000000454}, {
        "x": 358143.998,
        "y": 6678789.489,
        "z": 52.51799999999639
      }, {"x": 358169.157, "y": 6678799.552, "z": 52.90700000000652}, {
        "x": 358173.807,
        "y": 6678800.932,
        "z": 52.971999999994296
      }, {"x": 358201.092, "y": 6678806.254, "z": 53.245999999999185}, {
        "x": 358219.989,
        "y": 6678807.928,
        "z": 53.270999999993364
      }],
      "id": 454829,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1571,
      "endMValue": 124.7915184171252,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889230,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717175,
      "startAddressM": 2125,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454839,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357544.591, "y": 6678449.335, "z": 36.60099999999511}, {
        "x": 357546.056,
        "y": 6678449.946,
        "z": 36.578999999997905
      }, {"x": 357574.299, "y": 6678462.93, "z": 35.86000000000058}, {
        "x": 357610.645,
        "y": 6678484.382,
        "z": 34.65600000000268
      }, {"x": 357630.374, "y": 6678496.722, "z": 34.03100000000268}],
      "id": 454839,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2223,
      "endMValue": 98.1467400433594,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889422,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717408,
      "startAddressM": 339,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454814,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359243.445, "y": 6679056.642, "z": 49.721999999994296}, {
        "x": 359254.43,
        "y": 6679058.272,
        "z": 49.90700000000652
      }],
      "id": 454814,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 350,
      "endMValue": 11.105274647631582,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1004798332,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717278,
      "startAddressM": 17,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454810,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359508.274, "y": 6679181.17, "z": 54.570000000006985}, {
        "x": 359522.725,
        "y": 6679189.598,
        "z": 54.85000000000582
      }, {"x": 359540.682, "y": 6679199.184, "z": 55.00100000000384}],
      "id": 454810,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 54,
      "endMValue": 37.084566082900665,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905713,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717680,
      "startAddressM": 3274,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454846,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357278.592, "y": 6677475.9, "z": 37.15099999999802}, {
        "x": 357280.444,
        "y": 6677481.376,
        "z": 37.346999999994296
      }, {"x": 357287.035, "y": 6677505.669, "z": 38.187000000005355}, {
        "x": 357292.516,
        "y": 6677538.089,
        "z": 39.20799999999872
      }, {"x": 357294.979, "y": 6677566.02, "z": 39.846000000005006}, {
        "x": 357295.025,
        "y": 6677567.037,
        "z": 39.861999999993714
      }],
      "id": 454846,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3367,
      "endMValue": 92.88941131120599,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889554,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717139,
      "startAddressM": 1619,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454831,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358061.84, "y": 6678729.908, "z": 49.794999999998254}, {
        "x": 358067.597,
        "y": 6678735.807,
        "z": 50.029999999998836
      }],
      "id": 454831,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1627,
      "endMValue": 8.242648239629556,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889176,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717142,
      "startAddressM": 1696,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454833,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357950.142, "y": 6678634.535, "z": 45.2390000000014}, {
        "x": 357968.741,
        "y": 6678644.877,
        "z": 46.021999999997206
      }, {"x": 357990.839, "y": 6678659.757, "z": 46.93399999999383}, {
        "x": 358013.43,
        "y": 6678678.063,
        "z": 47.75100000000384
      }, {"x": 358015.136, "y": 6678679.667, "z": 47.812000000005355}],
      "id": 454833,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1775,
      "endMValue": 79.3403223352898,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889272,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717180,
      "startAddressM": 2473,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454842,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357288.493, "y": 6678276.228, "z": 39.205000000001746}, {
        "x": 357296.279,
        "y": 6678299.502,
        "z": 39.82499999999709
      }, {"x": 357309.364, "y": 6678327.163, "z": 40.21700000000419}, {
        "x": 357323.683,
        "y": 6678349.106,
        "z": 40.288000000000466
      }],
      "id": 454842,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2554,
      "endMValue": 81.3433237557588,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889668,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717388,
      "startAddressM": 1148,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454827,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358389.151, "y": 6678807.92, "z": 54.30800000000454}, {
        "x": 358391.028,
        "y": 6678808.503,
        "z": 54.35599999999977
      }, {"x": 358418.911, "y": 6678818.572, "z": 55.33400000000256}, {
        "x": 358442.716,
        "y": 6678828.838,
        "z": 56.11699999999837
      }, {"x": 358468.899, "y": 6678842.295, "z": 56.711999999999534}, {
        "x": 358494.524,
        "y": 6678860.405,
        "z": 57.05199999999604
      }, {"x": 358501.075, "y": 6678864.95, "z": 57.11599999999453}],
      "id": 454827,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1275,
      "endMValue": 126.32562941484822,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889248,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717418,
      "startAddressM": 647,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454818,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358912.321, "y": 6679002.109, "z": 52.14100000000326}, {
        "x": 358929.312,
        "y": 6679003.692,
        "z": 52.11800000000221
      }, {"x": 358951.67, "y": 6679007.532, "z": 52.177999999999884}],
      "id": 454818,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 687,
      "endMValue": 39.749946860184096,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905851,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717365,
      "startAddressM": 931,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454822,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358647.408, "y": 6678984.55, "z": 56.31399999999849}, {
        "x": 358657.006,
        "y": 6678990.713,
        "z": 55.88999999999942
      }, {"x": 358671.441, "y": 6678998.552, "z": 55.45699999999488}],
      "id": 454822,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 959,
      "endMValue": 27.832494215951016,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905815,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717173,
      "startAddressM": 2043,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454838,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 357630.374, "y": 6678496.722, "z": 34.03100000000268}, {
        "x": 357645.029,
        "y": 6678505.889,
        "z": 33.728000000002794
      }, {"x": 357683.221, "y": 6678528.429, "z": 33.47699999999895}, {
        "x": 357700.735,
        "y": 6678537.673,
        "z": 33.53599999999278
      }],
      "id": 454838,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2125,
      "endMValue": 81.43701909571836,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889500,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717370,
      "startAddressM": 687,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454819,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 358912.321, "y": 6679002.109, "z": 52.14100000000326}, {
        "x": 358903.195,
        "y": 6679001.586,
        "z": 52.16800000000512
      }, {"x": 358874.325, "y": 6679000.933, "z": 52.388999999995576}, {
        "x": 358849.336,
        "y": 6679002.254,
        "z": 52.68099999999686
      }, {"x": 358836.015, "y": 6679003.415, "z": 52.90600000000268}],
      "id": 454819,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 763,
      "endMValue": 76.4137479481388,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906001,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717367,
      "startAddressM": 763,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454820,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 358836.015, "y": 6679003.415, "z": 52.90600000000268}, {
        "x": 358824.341,
        "y": 6679004.716,
        "z": 53.17500000000291
      }, {"x": 358797.663, "y": 6679008.402, "z": 53.70900000000256}, {
        "x": 358771.834,
        "y": 6679011.027,
        "z": 54.179000000003725
      }, {"x": 358766.935, "y": 6679011.54, "z": 54.229000000006636}],
      "id": 454820,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 833,
      "endMValue": 69.56554029601783,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906007,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717409,
      "startAddressM": 54,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454811,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359423.219, "y": 6679130.725, "z": 52.095000000001164}, {
        "x": 359447.657,
        "y": 6679145.517,
        "z": 52.870999999999185
      }, {"x": 359452.178, "y": 6679148.183, "z": 53.00699999999779}, {
        "x": 359503.315,
        "y": 6679178.277,
        "z": 54.46600000000035
      }, {"x": 359508.274, "y": 6679181.17, "z": 54.570000000006985}],
      "id": 454811,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 153,
      "endMValue": 98.8907447112789,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1209684997,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717161,
      "startAddressM": 1571,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454830,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358067.597, "y": 6678735.807, "z": 50.029999999998836}, {
        "x": 358089.264,
        "y": 6678755.504,
        "z": 50.85599999999977
      }, {"x": 358103.914, "y": 6678766.914, "z": 51.38199999999779}],
      "id": 454830,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1619,
      "endMValue": 47.850994038635534,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362892481,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717121,
      "startAddressM": 1275,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454828,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 358389.151, "y": 6678807.92, "z": 54.30800000000454}, {
        "x": 358365.86,
        "y": 6678802.388,
        "z": 53.7390000000014
      }, {"x": 358339.189, "y": 6678800.01, "z": 53.30800000000454}, {
        "x": 358310.537,
        "y": 6678800.629,
        "z": 53.205000000001746
      }, {"x": 358308.485, "y": 6678800.788, "z": 53.205000000001746}, {
        "x": 358297.544,
        "y": 6678802.09,
        "z": 53.195999999996275
      }, {"x": 358266.703, "y": 6678806.28, "z": 53.177999999999884}, {
        "x": 358235.263,
        "y": 6678808.304,
        "z": 53.20299999999406
      }, {"x": 358219.989, "y": 6678807.928, "z": 53.270999999993364}],
      "id": 454828,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1446,
      "endMValue": 170.35882475616452,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1003607685,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718034,
      "startAddressM": 5840,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454858,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 354738.98, "y": 6676806.867, "z": 37.33999999999651}, {
        "x": 354746.53,
        "y": 6676806.917,
        "z": 37.40700000000652
      }, {"x": 354777.684, "y": 6676807.272, "z": 37.48399999999674}, {
        "x": 354805.462,
        "y": 6676807.139,
        "z": 37.25500000000466
      }, {"x": 354831.137, "y": 6676806.955, "z": 36.86800000000221}, {
        "x": 354888.868,
        "y": 6676807.159,
        "z": 35.80199999999604
      }, {"x": 354997.05, "y": 6676807.026, "z": 33.53299999999581}, {
        "x": 355064.937,
        "y": 6676807.042,
        "z": 32.19000000000233
      }],
      "id": 454858,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6166,
      "endMValue": 325.9606098868213,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362891048,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718080,
      "startAddressM": 5459,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454857,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 355064.937, "y": 6676807.042, "z": 32.19000000000233}, {
        "x": 355200.106,
        "y": 6676807.073,
        "z": 30.055999999996857
      }, {"x": 355310.814, "y": 6676807.091, "z": 31.09100000000035}, {
        "x": 355343.154,
        "y": 6676806.987,
        "z": 31.94999999999709
      }, {"x": 355370.758, "y": 6676807.038, "z": 32.70799999999872}, {
        "x": 355394.829,
        "y": 6676807.325,
        "z": 33.54099999999744
      }, {"x": 355444.857, "y": 6676807.148, "z": 35.052999999999884}],
      "id": 454857,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5840,
      "endMValue": 379.9222433658031,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890994,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718086,
      "startAddressM": 5371,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454855,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 355514.941, "y": 6676807.053, "z": 36.379000000000815}, {
        "x": 355518.133,
        "y": 6676807.105,
        "z": 36.42399999999907
      }, {"x": 355529.135, "y": 6676807.551, "z": 36.562999999994645}, {
        "x": 355531.544,
        "y": 6676807.611,
        "z": 36.59900000000198
      }, {"x": 355532.258, "y": 6676807.614, "z": 36.604999999995925}],
      "id": 454855,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5389,
      "endMValue": 17.32721319725294,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890958,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1718084,
      "startAddressM": 5389,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454856,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 355514.941, "y": 6676807.053, "z": 36.379000000000815}, {
        "x": 355493.773,
        "y": 6676807.187,
        "z": 36.04300000000512
      }, {"x": 355474.641, "y": 6676807.456, "z": 35.71700000000419}, {
        "x": 355445.828,
        "y": 6676807.145,
        "z": 35.070999999996275
      }, {"x": 355444.857, "y": 6676807.148, "z": 35.052999999999884}],
      "id": 454856,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5459,
      "endMValue": 70.08799814363621,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890928,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717711,
      "startAddressM": 4206,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454849,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 356377.335, "y": 6676889.867, "z": 40.95600000000559}, {
        "x": 356381.725,
        "y": 6676890.86,
        "z": 40.861000000004424
      }, {"x": 356414.805, "y": 6676900.644, "z": 40.04399999999441}, {
        "x": 356444.661,
        "y": 6676913.395,
        "z": 39.28900000000431
      }, {"x": 356473.042, "y": 6676928.008, "z": 38.88999999999942}, {
        "x": 356502.507,
        "y": 6676945.214,
        "z": 38.61999999999534
      }, {"x": 356533.228, "y": 6676963.907, "z": 38.53200000000652}, {
        "x": 356558.372,
        "y": 6676979.254,
        "z": 38.695999999996275
      }, {"x": 356585.606, "y": 6676995.447, "z": 38.961999999999534}, {
        "x": 356613.301,
        "y": 6677010.925,
        "z": 39.255999999993946
      }, {"x": 356650.006, "y": 6677025.968, "z": 39.611999999993714}, {
        "x": 356653.366,
        "y": 6677027.058,
        "z": 39.653000000005704
      }],
      "id": 454849,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4516,
      "endMValue": 309.53561024855384,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890088,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717716,
      "startAddressM": 4711,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454851,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 356069.1, "y": 6676821.197, "z": 42.39800000000105}, {
        "x": 356080.61,
        "y": 6676823.137,
        "z": 42.595000000001164
      }, {"x": 356108.013, "y": 6676828.741, "z": 42.9890000000014}, {
        "x": 356136.228,
        "y": 6676835.097,
        "z": 43.17200000000594
      }, {"x": 356156.711, "y": 6676839.867, "z": 43.288000000000466}, {
        "x": 356181.251,
        "y": 6676845.257,
        "z": 43.44000000000233
      }, {"x": 356188.3, "y": 6676846.878, "z": 43.42299999999523}],
      "id": 454851,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4833,
      "endMValue": 121.953567582042,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890106,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717631,
      "startAddressM": 3957,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454848,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 356653.366, "y": 6677027.058, "z": 39.653000000005704}, {
        "x": 356687.867,
        "y": 6677036.348,
        "z": 39.98099999999977
      }, {"x": 356723.428, "y": 6677043.55, "z": 40.24700000000303}, {
        "x": 356763.399,
        "y": 6677051.272,
        "z": 40.495999999999185
      }, {"x": 356795.085, "y": 6677057.348, "z": 40.57600000000093}, {
        "x": 356874.365,
        "y": 6677071.781,
        "z": 40.52300000000105
      }, {"x": 356896.912, "y": 6677075.878, "z": 40.520000000004075}],
      "id": 454848,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4206,
      "endMValue": 248.48546981754026,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362889878,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717723,
      "startAddressM": 5044,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454853,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 355847.471, "y": 6676807.376, "z": 38.06600000000617}, {
        "x": 355858.645,
        "y": 6676807.4,
        "z": 38.255999999993946
      }],
      "id": 454853,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5056,
      "endMValue": 11.17402577408828,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890124,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717719,
      "startAddressM": 4833,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454852,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 355858.645, "y": 6676807.4, "z": 38.255999999993946}, {
        "x": 355878.501,
        "y": 6676807.443,
        "z": 38.654999999998836
      }, {"x": 355912.404, "y": 6676807.587, "z": 39.36000000000058}, {
        "x": 355948.142,
        "y": 6676808.97,
        "z": 40.078999999997905
      }, {"x": 355978.006, "y": 6676810.243, "z": 40.61999999999534}, {
        "x": 356011.325,
        "y": 6676813.173,
        "z": 41.254000000000815
      }, {"x": 356045.305, "y": 6676817.657, "z": 41.91000000000349}, {
        "x": 356069.1,
        "y": 6676821.197,
        "z": 42.39800000000105
      }],
      "id": 454852,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5044,
      "endMValue": 211.19426304511572,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890118,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717724,
      "startAddressM": 4516,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454850,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 356188.3, "y": 6676846.878, "z": 43.42299999999523}, {
        "x": 356192.978,
        "y": 6676847.954,
        "z": 43.455000000001746
      }, {"x": 356213.559, "y": 6676852.532, "z": 43.45600000000559}, {
        "x": 356240.236,
        "y": 6676858.642,
        "z": 43.28599999999278
      }, {"x": 356277.079, "y": 6676866.482, "z": 42.86699999999837}, {
        "x": 356315.974,
        "y": 6676875.355,
        "z": 42.229000000006636
      }, {"x": 356353.57, "y": 6676884.496, "z": 41.5109999999986}, {
        "x": 356377.335,
        "y": 6676889.867,
        "z": 40.95600000000559
      }],
      "id": 454850,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4711,
      "endMValue": 193.86978113366462,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890094,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1718094,
      "startAddressM": 5056,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "administrativeClassMML": "State",
      "segmentId": 454854,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 355603.474, "y": 6676807.33, "z": 36.86500000000524}, {
        "x": 355634.341,
        "y": 6676807.401,
        "z": 36.861999999993714
      }, {"x": 355711.305, "y": 6676807.481, "z": 37.0109999999986}, {
        "x": 355792.49,
        "y": 6676807.499,
        "z": 37.31699999999546
      }, {"x": 355838.321, "y": 6676807.356, "z": 37.91800000000512}, {
        "x": 355847.471,
        "y": 6676807.376,
        "z": 38.06600000000617
      }],
      "id": 454854,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5300,
      "endMValue": 243.9973701787339,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362890904,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716825,
      "startAddressM": 93,
      "roadNameFi": "Kurkistontie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 25865,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11283,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356817.606, "y": 6682462.855, "z": 49.15799999999581}, {
        "x": 356810.967,
        "y": 6682476.835,
        "z": 49.19000000000233
      }, {"x": 356804.826, "y": 6682491.798, "z": 49.08100000000559}, {
        "x": 356800.085,
        "y": 6682507.045,
        "z": 48.82700000000477
      }, {"x": 356798.072, "y": 6682526.329, "z": 48.463000000003376}, {
        "x": 356800.236,
        "y": 6682542.718,
        "z": 48.262000000002445
      }, {"x": 356801.436, "y": 6682564.767, "z": 48.3579999999929}, {
        "x": 356802.037,
        "y": 6682589.517,
        "z": 48.54700000000594
      }, {"x": 356798.438, "y": 6682608.866, "z": 48.62200000000303}, {
        "x": 356794.24,
        "y": 6682622.367,
        "z": 49.00199999999313
      }, {"x": 356786.602, "y": 6682640.481, "z": 49.91099999999278}, {
        "x": 356778.439,
        "y": 6682662.128,
        "z": 51.495999999999185
      }, {"x": 356773.065, "y": 6682679.603, "z": 52.58900000000722}, {
        "x": 356771.599,
        "y": 6682694.137,
        "z": 52.721000000005006
      }, {"x": 356774.205, "y": 6682709.335, "z": 51.79200000000128}, {
        "x": 356781.018,
        "y": 6682723.492,
        "z": 50.23399999999674
      }, {"x": 356794.113, "y": 6682744.18, "z": 48.687999999994645}],
      "id": 25865,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 396,
      "endMValue": 295.495,
      "roadNameSe": "Kurkistovägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356053845,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716832,
      "startAddressM": 0,
      "roadNameFi": "Kurkistontie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 8394,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 11283,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 356898.016, "y": 6682442.384, "z": 51.91000000000349}, {
        "x": 356886.743,
        "y": 6682442.896,
        "z": 51.66000000000349
      }, {"x": 356873.76, "y": 6682442.324, "z": 50.76799999999639}, {
        "x": 356859.043,
        "y": 6682440.17,
        "z": 49.11000000000058
      }, {"x": 356846.716, "y": 6682439.401, "z": 48.13199999999779}, {
        "x": 356838.035,
        "y": 6682441.505,
        "z": 48.187999999994645
      }, {"x": 356827.421, "y": 6682449.694, "z": 48.7670000000071}, {
        "x": 356817.60600877483,
        "y": 6682462.854988234,
        "z": 49.157999650434334
      }],
      "id": 8394,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 93,
      "endMValue": 90.261,
      "roadNameSe": "Kurkistovägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 356898.016, "y": 6682442.384, "z": 51.91000000000349}, "value": 0}],
      "mmlId": 356053839,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150432,
      "startAddressM": 2582,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 37428,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363003.847, "y": 6682187.127, "z": 69.403999999995}, {
        "x": 362979.696,
        "y": 6682198.495,
        "z": 67.88999999999942
      }],
      "id": 37428,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2609,
      "endMValue": 26.693,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083803,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150563,
      "startAddressM": 3694,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 8299,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 361954.858, "y": 6682514.611, "z": 61.25}, {
        "x": 361942.392,
        "y": 6682521.584,
        "z": 61.25500000000466
      }, {"x": 361926.965, "y": 6682530.686, "z": 58.86800000000221}],
      "id": 8299,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3726,
      "endMValue": 32.196,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083833,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.01.2016 23:00:05",
      "linkId": 150338,
      "startAddressM": 2609,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 53850,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 362979.696, "y": 6682198.495, "z": 67.88999999999942}, {
        "x": 362973.239,
        "y": 6682201.535,
        "z": 69.65099999999802
      }, {"x": 362923.287, "y": 6682225.334, "z": 70.19000000000233}, {
        "x": 362867.56,
        "y": 6682250.861,
        "z": 70.81500000000233
      }, {"x": 362813.229, "y": 6682275.051, "z": 71.36999999999534}, {
        "x": 362763.271,
        "y": 6682295.825,
        "z": 71.85000000000582
      }, {"x": 362710.3, "y": 6682317.199, "z": 72.32499999999709}, {
        "x": 362653.722,
        "y": 6682338.948,
        "z": 72.6820000000007
      }, {"x": 362598.274, "y": 6682358.181, "z": 72.93700000000536}, {
        "x": 362549.974,
        "y": 6682373.404,
        "z": 73.01799999999639
      }, {"x": 362499.538, "y": 6682385.828, "z": 72.82200000000012}, {
        "x": 362448.875,
        "y": 6682396.001,
        "z": 72.45200000000477
      }, {"x": 362401.653, "y": 6682403.491, "z": 71.93300000000454}, {
        "x": 362339.932,
        "y": 6682409.694,
        "z": 70.98500000000058
      }, {"x": 362284.482, "y": 6682415.169, "z": 70.01600000000326}, {
        "x": 362232.834,
        "y": 6682422.065,
        "z": 68.8469999999943
      }, {"x": 362212.974, "y": 6682425.102, "z": 68.35199999999895}, {
        "x": 362182.927,
        "y": 6682430.691,
        "z": 67.48099999999977
      }, {"x": 362144.942, "y": 6682439.437, "z": 66.32300000000396}, {
        "x": 362109.681,
        "y": 6682449.245,
        "z": 65.06900000000314
      }, {"x": 362074.494, "y": 6682460.716, "z": 63.90600000000268}, {
        "x": 362042.197,
        "y": 6682473.004,
        "z": 62.99800000000687
      }, {"x": 362007.644, "y": 6682487.948, "z": 62.137000000002445}, {
        "x": 361972.078,
        "y": 6682505.435,
        "z": 61.49199999999837
      }, {"x": 361954.858, "y": 6682514.611, "z": 61.25}],
      "id": 53850,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3694,
      "endMValue": 1080.284,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083839,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.01.2016 23:00:05",
      "linkId": 150407,
      "startAddressM": 1917,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 66757,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363580.2, "y": 6681860.475, "z": 63.05999999999767}, {
        "x": 363519.149,
        "y": 6681900.374,
        "z": 64.1820000000007
      }, {"x": 363466.843, "y": 6681933.755, "z": 65.00900000000547}, {
        "x": 363416.144,
        "y": 6681965.017,
        "z": 65.76799999999639
      }, {"x": 363362.756, "y": 6681997.083, "z": 66.47699999999895}, {
        "x": 363309.971,
        "y": 6682027.418,
        "z": 67.08599999999569
      }, {"x": 363262.574, "y": 6682053.451, "z": 67.52700000000186}, {
        "x": 363209.965,
        "y": 6682083.067,
        "z": 68.03399999999965
      }, {"x": 363159.08, "y": 6682110.25, "z": 68.37900000000081}, {
        "x": 363115.473,
        "y": 6682132.674,
        "z": 68.65799999999581
      }, {"x": 363065.532, "y": 6682158.111, "z": 68.95799999999872}, {
        "x": 363021.318,
        "y": 6682178.904,
        "z": 69.28399999999965
      }, {"x": 363003.847, "y": 6682187.127, "z": 69.403999999995}],
      "id": 66757,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2582,
      "endMValue": 663.092,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083881,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150414,
      "startAddressM": 1894,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 18428,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 363598.684, "y": 6681848.289, "z": 62.695000000006985}, {
        "x": 363580.20040597545,
        "y": 6681860.4747323515,
        "z": 63.059991983279104
      }],
      "id": 18428,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1917,
      "endMValue": 22.139,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083887,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 150404,
      "startAddressM": 839,
      "roadNameFi": "Turunväylä",
      "roadPartNumber": 7,
      "administrativeClassMML": "State",
      "segmentId": 23185,
      "municipalityCode": 49,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 364529.512, "y": 6681365.784, "z": 51.470000000001164}, {
        "x": 364503.518,
        "y": 6681374.968,
        "z": 51.75100000000384
      }, {"x": 364415.291, "y": 6681408.198, "z": 52.520000000004075}, {
        "x": 364350.729,
        "y": 6681434.092,
        "z": 53.179999999993015
      }, {"x": 364278.31, "y": 6681465.039, "z": 53.90099999999802}, {
        "x": 364199.821,
        "y": 6681500.174,
        "z": 54.69999999999709
      }, {"x": 364145.188, "y": 6681526.399, "z": 55.27599999999802}, {
        "x": 364085.715,
        "y": 6681555.191,
        "z": 55.85700000000361
      }, {"x": 364041.873, "y": 6681577.795, "z": 56.28299999999581}, {
        "x": 363981.039,
        "y": 6681611.084,
        "z": 56.903000000005704
      }, {"x": 363926.075, "y": 6681641.498, "z": 57.49099999999453}, {
        "x": 363867.821,
        "y": 6681674.147,
        "z": 58.20699999999488
      }, {"x": 363820.37, "y": 6681703.437, "z": 58.838000000003376}, {
        "x": 363766.194,
        "y": 6681736.792,
        "z": 59.68300000000454
      }, {"x": 363760.267, "y": 6681740.718, "z": 59.78100000000268}, {
        "x": 363713.052,
        "y": 6681771.988,
        "z": 60.50699999999779
      }, {"x": 363661.849, "y": 6681806.489, "z": 61.445999999996275}, {
        "x": 363614.377,
        "y": 6681837.944,
        "z": 62.380999999993946
      }, {"x": 363598.684, "y": 6681848.289, "z": 62.695000000006985}],
      "id": 23185,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1894,
      "endMValue": 1051.712,
      "roadNameSe": "Åboleden",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356148797,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717274,
      "startAddressM": 5342,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 17301,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359542.672, "y": 6679396.28, "z": 58.98500000000058}, {
        "x": 359544.019,
        "y": 6679409.782,
        "z": 59.711999999999534
      }, {"x": 359546.601, "y": 6679431.052, "z": 60.80599999999686}, {
        "x": 359548.268,
        "y": 6679449.925,
        "z": 61.67399999999907
      }, {"x": 359548.896, "y": 6679461.464, "z": 62.096999999994296}, {
        "x": 359548.958,
        "y": 6679462.053,
        "z": 62.10899999999674
      }],
      "id": 17301,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5408,
      "endMValue": 66.09,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362906379,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716690,
      "startAddressM": 2030,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 32898,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358876.803, "y": 6682075.952, "z": 48.74899999999616}, {
        "x": 358887.165,
        "y": 6682091.279,
        "z": 48.95200000000477
      }, {"x": 358898.218, "y": 6682109.549, "z": 49.3920000000071}, {
        "x": 358905.245,
        "y": 6682121.425,
        "z": 49.895999999993364
      }, {"x": 358910.968, "y": 6682136.561, "z": 50.45299999999406}, {
        "x": 358914.156,
        "y": 6682150.235,
        "z": 50.92200000000594
      }, {"x": 358916.234, "y": 6682165.536, "z": 51.388999999995576}, {
        "x": 358916.154,
        "y": 6682181.134,
        "z": 51.520000000004075
      }, {"x": 358914.766, "y": 6682196.2, "z": 51.45100000000093}, {
        "x": 358911.8030415591,
        "y": 6682217.849696336,
        "z": 51.41900044882689
      }],
      "id": 32898,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2182,
      "endMValue": 151.897,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082171,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716799,
      "startAddressM": 3927,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 57035,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359300.633, "y": 6680713.69, "z": 58.10700000000361}, {
        "x": 359298.016,
        "y": 6680724.365,
        "z": 58.00699999999779
      }, {"x": 359293.045, "y": 6680739.98, "z": 57.47599999999511}, {
        "x": 359287.031,
        "y": 6680753.511,
        "z": 56.56900000000314
      }, {"x": 359279.332, "y": 6680766.544, "z": 55.570000000006985}, {
        "x": 359271.298,
        "y": 6680777.332,
        "z": 54.90600000000268
      }, {"x": 359269.943, "y": 6680778.739, "z": 54.80100000000675}],
      "id": 57035,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3999,
      "endMValue": 72.727,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082615,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717272,
      "startAddressM": 5408,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 54279,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359555.613, "y": 6679206.958, "z": 54.98399999999674}, {
        "x": 359552.727,
        "y": 6679215.008,
        "z": 55.14100000000326
      }, {"x": 359545.284, "y": 6679240.118, "z": 55.41800000000512}, {
        "x": 359539.772,
        "y": 6679266.637,
        "z": 55.61699999999837
      }, {"x": 359536.633, "y": 6679290.355, "z": 55.875}, {
        "x": 359535.733,
        "y": 6679314.549,
        "z": 56.211999999999534
      }, {"x": 359536.726, "y": 6679337.526, "z": 56.67500000000291}, {
        "x": 359538.941,
        "y": 6679362.92,
        "z": 57.42200000000594
      }, {"x": 359541.536, "y": 6679386.822, "z": 58.494000000006054}, {
        "x": 359542.6719742652,
        "y": 6679396.27978574,
        "z": 58.984988876942666
      }],
      "id": 54279,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5600,
      "endMValue": 192.02,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 359555.613, "y": 6679206.958, "z": 54.98399999999674}, "value": 5600}],
      "mmlId": 362905197,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896971,
      "startAddressM": 5141,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 24462,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359550.931, "y": 6679483.012, "z": 62.48099999999977}, {
        "x": 359552.001,
        "y": 6679497.809,
        "z": 62.41599999999744
      }, {"x": 359550.854, "y": 6679520.068, "z": 62.01600000000326}, {
        "x": 359548.968,
        "y": 6679543.366,
        "z": 61.25100000000384
      }, {"x": 359545.821, "y": 6679566.793, "z": 59.87300000000687}, {
        "x": 359542.611,
        "y": 6679587.97,
        "z": 58.50500000000466
      }, {"x": 359542.073, "y": 6679591.432, "z": 58.25699999999779}, {
        "x": 359537.088,
        "y": 6679618.959,
        "z": 56.18899999999849
      }, {"x": 359534.583, "y": 6679632.933, "z": 55.286999999996624}, {
        "x": 359532.325,
        "y": 6679644.849,
        "z": 54.56600000000617
      }, {"x": 359528.7380423694, "y": 6679660.921810147, "z": 53.784009236930935}],
      "id": 24462,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5320,
      "endMValue": 179.826,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 945007488,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716675,
      "startAddressM": 1644,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 29571,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358888.884, "y": 6682339.273, "z": 54.49000000000524}, {
        "x": 358887.758,
        "y": 6682352.531,
        "z": 55.18099999999686
      }, {"x": 358887.052, "y": 6682366.87, "z": 55.771999999997206}, {
        "x": 358885.609,
        "y": 6682381.971,
        "z": 56.29700000000594
      }, {"x": 358883.076, "y": 6682397.854, "z": 56.84299999999348}, {
        "x": 358877.952,
        "y": 6682417.524,
        "z": 57.64400000000023
      }, {"x": 358872.174, "y": 6682432.815, "z": 58.504000000000815}, {
        "x": 358864.582,
        "y": 6682448.882,
        "z": 59.513999999995576
      }, {"x": 358855.316, "y": 6682463.25, "z": 60.34100000000035}, {
        "x": 358841.03,
        "y": 6682480.853,
        "z": 60.838000000003376
      }, {"x": 358827.061, "y": 6682495.175, "z": 60.721999999994296}, {
        "x": 358817.467,
        "y": 6682506.498,
        "z": 60.50500000000466
      }, {"x": 358809.763, "y": 6682515.748, "z": 60.328999999997905}, {
        "x": 358799.441,
        "y": 6682531.846,
        "z": 60.09299999999348
      }, {"x": 358792.556, "y": 6682544.536, "z": 59.929000000003725}, {
        "x": 358786.604,
        "y": 6682559.685,
        "z": 59.75900000000547
      }, {"x": 358782.17812906683, "y": 6682571.321660653, "z": 59.70400160385544}],
      "id": 29571,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1906,
      "endMValue": 262.298,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356084025,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717212,
      "startAddressM": 5086,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 60258,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359528.738, "y": 6679660.922, "z": 53.78399999999965}, {
        "x": 359527.042,
        "y": 6679667.396,
        "z": 53.528000000005704
      }, {"x": 359518.947, "y": 6679691.461, "z": 53.00199999999313}, {
        "x": 359509.691,
        "y": 6679711.92,
        "z": 52.763999999995576
      }],
      "id": 60258,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5141,
      "endMValue": 54.538,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905929,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716757,
      "startAddressM": 3043,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 19185,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358764.215, "y": 6681357.076, "z": 48.31500000000233}, {
        "x": 358760.154,
        "y": 6681359.447,
        "z": 48.28900000000431
      }, {"x": 358743.022, "y": 6681369.154, "z": 48.22299999999814}, {
        "x": 358739.189,
        "y": 6681371.407,
        "z": 48.20600000000559
      }, {"x": 358724.566, "y": 6681379.999, "z": 48.195999999996275}, {
        "x": 358703.565,
        "y": 6681392.638,
        "z": 48.18099999999686
      }, {"x": 358691.072, "y": 6681401.284, "z": 48.30000000000291}],
      "id": 19185,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3128,
      "endMValue": 85.504,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063369,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716766,
      "startAddressM": 2792,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 41459,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358629.168, "y": 6681463.591, "z": 49.9320000000007}, {
        "x": 358626.588,
        "y": 6681467.201,
        "z": 49.97699999999895
      }, {"x": 358615.911, "y": 6681483.247, "z": 50.23399999999674}, {
        "x": 358601.96,
        "y": 6681504.826,
        "z": 50.80000000000291
      }, {"x": 358592.289, "y": 6681518.073, "z": 51.21799999999348}, {
        "x": 358581.637,
        "y": 6681532.084,
        "z": 51.46400000000722
      }, {"x": 358571.092, "y": 6681544.535, "z": 51.34399999999732}, {
        "x": 358559.494,
        "y": 6681557.515,
        "z": 50.953999999997905
      }, {"x": 358549.598, "y": 6681569.757, "z": 50.63999999999942}, {
        "x": 358542.676,
        "y": 6681585.508,
        "z": 50.312999999994645
      }, {"x": 358540.1090731278, "y": 6681597.348662678, "z": 50.18000378885186}],
      "id": 41459,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2954,
      "endMValue": 162.194,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063489,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716682,
      "startAddressM": 1906,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 56506,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358892.707, "y": 6682316.142, "z": 53.19999999999709}, {
        "x": 358890.431,
        "y": 6682328.353,
        "z": 53.8640000000014
      }, {"x": 358888.8840469822, "y": 6682339.272668361, "z": 54.48998098845652}],
      "id": 56506,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1929,
      "endMValue": 23.45,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082099,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716786,
      "startAddressM": 3762,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 63987,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359210.974, "y": 6680837.053, "z": 52.096000000005006}, {
        "x": 359206.234,
        "y": 6680841.52,
        "z": 51.978000000002794
      }, {"x": 359195.872, "y": 6680853.027, "z": 51.754000000000815}, {
        "x": 359186.107,
        "y": 6680866.807,
        "z": 51.570999999996275
      }, {"x": 359178.575, "y": 6680880.833, "z": 51.43300000000454}, {
        "x": 359172.259,
        "y": 6680895.368,
        "z": 51.33299999999872
      }, {"x": 359169.0160240623, "y": 6680905.733923087, "z": 51.1430014097505}],
      "id": 63987,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3844,
      "endMValue": 81.517,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082315,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717217,
      "startAddressM": 5320,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 28760,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359548.958, "y": 6679462.053, "z": 62.10899999999674}, {
        "x": 359550.931,
        "y": 6679483.012,
        "z": 62.48099999999977
      }],
      "id": 28760,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5342,
      "endMValue": 21.052,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 945007514,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716797,
      "startAddressM": 3844,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 25070,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359269.943, "y": 6680778.739, "z": 54.80100000000675}, {
        "x": 359262.029,
        "y": 6680788.105,
        "z": 54.28500000000349
      }, {"x": 359248.828, "y": 6680802.216, "z": 53.562000000005355}, {
        "x": 359233.788,
        "y": 6680816.244,
        "z": 53.06500000000233
      }, {"x": 359216.703, "y": 6680831.873, "z": 52.34200000000419}, {
        "x": 359210.9743683051,
        "y": 6680837.052666989,
        "z": 52.096015814817605
      }],
      "id": 25070,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3927,
      "endMValue": 83.03,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082333,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717207,
      "startAddressM": 4635,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 26279,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359418.61, "y": 6679875.322, "z": 51.304999999993015}, {
        "x": 359414.461,
        "y": 6679881.007,
        "z": 50.971999999994296
      }, {"x": 359398.162, "y": 6679900.66, "z": 50.13400000000547}, {
        "x": 359380.825,
        "y": 6679917.418,
        "z": 49.58100000000559
      }, {"x": 359360.985, "y": 6679937.045, "z": 48.9829999999929}, {
        "x": 359346.229,
        "y": 6679954.545,
        "z": 48.788000000000466
      }, {"x": 359335.02, "y": 6679970.394, "z": 49.05199999999604}, {
        "x": 359323.441,
        "y": 6679990.898,
        "z": 49.59200000000419
      }, {"x": 359311.877, "y": 6680016.507, "z": 49.97699999999895}, {
        "x": 359304.291,
        "y": 6680039.882,
        "z": 50.10199999999895
      }, {"x": 359298.678, "y": 6680064.305, "z": 50.24099999999453}, {
        "x": 359292.749,
        "y": 6680099.701,
        "z": 50.02700000000186
      }],
      "id": 26279,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4899,
      "endMValue": 264.064,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082441,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716683,
      "startAddressM": 1929,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 38279,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358898.762, "y": 6682289.938, "z": 52.22699999999895}, {
        "x": 358895.188,
        "y": 6682305.023,
        "z": 52.687000000005355
      }, {"x": 358892.7070077753, "y": 6682316.141965154, "z": 53.19999839228431}],
      "id": 38279,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1956,
      "endMValue": 26.895,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082093,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1716806,
      "startAddressM": 4257,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 16854,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359319.843, "y": 6680389.769, "z": 54.179999999993015}, {
        "x": 359327.032,
        "y": 6680407.171,
        "z": 54.620999999999185
      }, {"x": 359334.138, "y": 6680426.771, "z": 54.89400000000023}, {
        "x": 359340.274,
        "y": 6680445.135,
        "z": 55.11999999999534
      }, {"x": 359344.246, "y": 6680462.135, "z": 55.34799999999814}],
      "id": 16854,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4334,
      "endMValue": 76.497,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082477,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896730,
      "startAddressM": 4159,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 46115,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359344.246, "y": 6680462.135, "z": 55.34799999999814}, {
        "x": 359346.941,
        "y": 6680479.193,
        "z": 55.54899999999907
      }, {"x": 359348.334, "y": 6680497.773, "z": 55.70200000000477}, {
        "x": 359347.539,
        "y": 6680516.452,
        "z": 55.87300000000687
      }, {"x": 359345.301, "y": 6680533.553, "z": 56.17600000000675}, {
        "x": 359341.844,
        "y": 6680549.977,
        "z": 56.445999999996275
      }, {"x": 359339.301, "y": 6680558.744, "z": 56.58299999999872}],
      "id": 46115,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4257,
      "endMValue": 97.757,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083557,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "15.12.2016 23:00:53",
      "linkId": 500027916,
      "startAddressM": 2752,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 4842,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358542.764, "y": 6681619.165, "z": 50.020000000004075}, {
        "x": 358544.262,
        "y": 6681625.611,
        "z": 49.96700000000419
      }, {"x": 358547.9769805219, "y": 6681636.612942316, "z": 49.87600047712594}],
      "id": 4842,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2770,
      "endMValue": 18.23,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1771239899,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716705,
      "startAddressM": 2700,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 61889,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358547.977, "y": 6681636.613, "z": 49.87600000000384}, {
        "x": 358555.177,
        "y": 6681655.171,
        "z": 49.720000000001164
      }, {"x": 358566.664, "y": 6681683.576, "z": 49.44899999999325}, {
        "x": 358567.185,
        "y": 6681684.801,
        "z": 49.43300000000454
      }],
      "id": 61889,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2752,
      "endMValue": 51.877,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063765,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716700,
      "startAddressM": 2182,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 42058,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358822.405, "y": 6681929.959, "z": 49.546000000002095}, {
        "x": 358823.298,
        "y": 6681932.012,
        "z": 49.471000000005006
      }, {"x": 358827.546, "y": 6681945.221, "z": 49.19400000000314}, {
        "x": 358830.176,
        "y": 6681961.398,
        "z": 49.14100000000326
      }, {"x": 358834.782, "y": 6681989.217, "z": 49.27700000000186}, {
        "x": 358838.641,
        "y": 6682004.636,
        "z": 49.24199999999837
      }, {"x": 358843.387, "y": 6682017.843, "z": 49.12600000000384}, {
        "x": 358850.802,
        "y": 6682033.298,
        "z": 48.945000000006985
      }, {"x": 358858.207, "y": 6682046.383, "z": 48.778999999994994}, {
        "x": 358865.545,
        "y": 6682058.975,
        "z": 48.66800000000512
      }, {"x": 358876.8029518952, "y": 6682075.951927458, "z": 48.74899965388792}],
      "id": 42058,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2339,
      "endMValue": 157.751,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082399,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716694,
      "startAddressM": 1956,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 22068,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358911.803, "y": 6682217.85, "z": 51.41899999999441}, {
        "x": 358909.243,
        "y": 6682235.338,
        "z": 51.44899999999325
      }, {"x": 358905.941, "y": 6682253.845, "z": 51.6140000000014}, {
        "x": 358902.574,
        "y": 6682270.975,
        "z": 51.88999999999942
      }, {"x": 358898.762, "y": 6682289.938, "z": 52.22699999999895}],
      "id": 22068,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2030,
      "endMValue": 73.274,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082081,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717208,
      "startAddressM": 4899,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 40136,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359477.827, "y": 6679772.109, "z": 53.29399999999441}, {
        "x": 359471.915,
        "y": 6679782.762,
        "z": 53.57799999999406
      }, {"x": 359461.613, "y": 6679800.606, "z": 53.896999999997206}, {
        "x": 359456.11,
        "y": 6679810.136,
        "z": 53.89100000000326
      }, {"x": 359441.13, "y": 6679837.176, "z": 53.19800000000396}, {
        "x": 359428.145,
        "y": 6679860.755,
        "z": 52.10899999999674
      }, {"x": 359418.61, "y": 6679875.322, "z": 51.304999999993015}],
      "id": 40136,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5018,
      "endMValue": 119.033,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356081643,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716704,
      "startAddressM": 2627,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 46458,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358567.185, "y": 6681684.801, "z": 49.43300000000454}, {
        "x": 358578.962,
        "y": 6681711.668,
        "z": 49.04700000000594
      }, {"x": 358590.604, "y": 6681733.993, "z": 48.695999999996275}, {
        "x": 358600.2387539854,
        "y": 6681749.728598206,
        "z": 48.27101085170238
      }],
      "id": 46458,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2700,
      "endMValue": 72.964,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 926508114,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896708,
      "startAddressM": 3128,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 44217,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358879.342, "y": 6681248.492, "z": 47.395999999993364}, {
        "x": 358872.175,
        "y": 6681257.903,
        "z": 47.42699999999604
      }, {"x": 358852.184, "y": 6681280.705, "z": 47.794999999998254}, {
        "x": 358833.494,
        "y": 6681300.987,
        "z": 48.28900000000431
      }, {"x": 358809.628, "y": 6681324.184, "z": 48.34100000000035}, {
        "x": 358795.465,
        "y": 6681335.558,
        "z": 48.40600000000268
      }, {"x": 358778.234, "y": 6681348.261, "z": 48.40899999999965}, {
        "x": 358770.79,
        "y": 6681352.951,
        "z": 48.3640000000014
      }, {"x": 358764.21517947514, "y": 6681357.075887402, "z": 48.315001337535406}],
      "id": 44217,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3287,
      "endMValue": 159.148,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 915228761,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896746,
      "startAddressM": 4146,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 16425,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359339.301, "y": 6680558.744, "z": 56.58299999999872}, {
        "x": 359335.262,
        "y": 6680572.027,
        "z": 56.80199999999604
      }],
      "id": 16425,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4159,
      "endMValue": 13.884,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1737616399,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716755,
      "startAddressM": 3548,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 55562,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359169.016, "y": 6680905.734, "z": 51.14299999999639}, {
        "x": 359164.464,
        "y": 6680924.462,
        "z": 50.63499999999476
      }, {"x": 359159.698, "y": 6680944.51, "z": 50.03299999999581}, {
        "x": 359152.436,
        "y": 6680972.4,
        "z": 49.25299999999697
      }, {"x": 359146.748, "y": 6680991.616, "z": 48.72500000000582}, {
        "x": 359139.906,
        "y": 6681009.639,
        "z": 48.44199999999546
      }, {"x": 359131.739, "y": 6681027.169, "z": 48.49099999999453}, {
        "x": 359122.169,
        "y": 6681043.952,
        "z": 48.48200000000361
      }, {"x": 359114.478, "y": 6681054.767, "z": 48.520999999993364}, {
        "x": 359103.352,
        "y": 6681066.26,
        "z": 48.61000000000058
      }, {"x": 359093.298, "y": 6681074.833, "z": 48.75800000000163}, {
        "x": 359081.802,
        "y": 6681081.772,
        "z": 48.98399999999674
      }, {"x": 359072.1250445604, "y": 6681087.023975816, "z": 49.20799896852923}],
      "id": 55562,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3762,
      "endMValue": 213.595,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082309,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716783,
      "startAddressM": 3287,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 8927,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358928.529, "y": 6681170.446, "z": 48.64800000000105}, {
        "x": 358918.101,
        "y": 6681188.718,
        "z": 48.55899999999383
      }, {"x": 358904.962, "y": 6681211.291, "z": 48.13499999999476}, {
        "x": 358890.275,
        "y": 6681233.752,
        "z": 47.53299999999581
      }, {"x": 358879.3422491824, "y": 6681248.491664049, "z": 47.39600312246525}],
      "id": 8927,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3380,
      "endMValue": 92.345,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356081691,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716804,
      "startAddressM": 3999,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 48939,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359313.058, "y": 6680646.026, "z": 57.83199999999488}, {
        "x": 359310.011,
        "y": 6680662.739,
        "z": 57.93300000000454
      }, {"x": 359306.66, "y": 6680682.778, "z": 57.96499999999651}, {
        "x": 359303.041,
        "y": 6680702.119,
        "z": 58.07300000000396
      }, {"x": 359300.63306369237, "y": 6680713.689693944, "z": 58.106999100692256}],
      "id": 48939,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4068,
      "endMValue": 68.801,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083545,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896743,
      "startAddressM": 4068,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 46036,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359322.819, "y": 6680613.608, "z": 57.55800000000454}, {
        "x": 359320.07,
        "y": 6680622.814,
        "z": 57.69800000000396
      }, {"x": 359313.058, "y": 6680646.026, "z": 57.83199999999488}],
      "id": 46036,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4102,
      "endMValue": 33.856,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1737616478,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896968,
      "startAddressM": 4334,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 42496,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359292.749, "y": 6680099.701, "z": 50.02700000000186}, {
        "x": 359289.264,
        "y": 6680120.283,
        "z": 49.87200000000303
      }, {"x": 359284.661, "y": 6680144.074, "z": 49.88300000000163}, {
        "x": 359278.492,
        "y": 6680165.83,
        "z": 49.869000000006054
      }, {"x": 359271.823, "y": 6680188.306, "z": 49.713000000003376}, {
        "x": 359268.526,
        "y": 6680205.591,
        "z": 49.620999999999185
      }, {"x": 359268.697, "y": 6680224.054, "z": 49.630999999993946}, {
        "x": 359270.754,
        "y": 6680241.528,
        "z": 49.87300000000687
      }, {"x": 359274.684, "y": 6680258.01, "z": 50.10199999999895}, {
        "x": 359281.532,
        "y": 6680278.964,
        "z": 50.445000000006985
      }, {"x": 359288.046, "y": 6680297.196, "z": 50.76799999999639}, {
        "x": 359293.035,
        "y": 6680311.442,
        "z": 51.00500000000466
      }, {"x": 359298.306, "y": 6680327.143, "z": 51.51900000000023}, {
        "x": 359305.897,
        "y": 6680350.375,
        "z": 52.33299999999872
      }, {"x": 359313.418, "y": 6680371.053, "z": 53.395000000004075}, {
        "x": 359319.843,
        "y": 6680389.769,
        "z": 54.179999999993015
      }],
      "id": 42496,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4635,
      "endMValue": 301.059,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082465,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 499896744,
      "startAddressM": 4102,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 23613,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359335.262, "y": 6680572.027, "z": 56.80199999999604}, {
        "x": 359331.171,
        "y": 6680586.561,
        "z": 56.99000000000524
      }, {"x": 359327.22, "y": 6680600.2, "z": 57.25199999999313}, {
        "x": 359322.81910788885,
        "y": 6680613.607671308,
        "z": 57.55799249853021
      }],
      "id": 23613,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4146,
      "endMValue": 43.41,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356083551,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716782,
      "startAddressM": 3380,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 52656,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359072.125, "y": 6681087.024, "z": 49.20799999999872}, {
        "x": 359051.481,
        "y": 6681096.421,
        "z": 49.60000000000582
      }, {"x": 359026.154, "y": 6681107.052, "z": 49.75800000000163}, {
        "x": 359000.252,
        "y": 6681117.854,
        "z": 49.61999999999534
      }, {"x": 358979.377, "y": 6681126.969, "z": 49.28299999999581}, {
        "x": 358960.618,
        "y": 6681137.739,
        "z": 48.8920000000071
      }, {"x": 358947.344, "y": 6681148.887, "z": 48.70100000000093}, {
        "x": 358928.529,
        "y": 6681170.446,
        "z": 48.64800000000105
      }],
      "id": 52656,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3548,
      "endMValue": 168.572,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356082297,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "15.12.2016 23:00:53",
      "linkId": 500027920,
      "startAddressM": 2770,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 68465,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358540.109, "y": 6681597.349, "z": 50.179999999993015}, {
        "x": 358541.134,
        "y": 6681608.813,
        "z": 50.04200000000128
      }, {"x": 358542.7639574217, "y": 6681619.164729589, "z": 50.02000057468119}],
      "id": 68465,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2792,
      "endMValue": 21.989,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063747,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716702,
      "startAddressM": 2339,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 14469,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358600.239, "y": 6681749.729, "z": 48.270999999993364}, {
        "x": 358611.561,
        "y": 6681761.549,
        "z": 47.56900000000314
      }, {"x": 358624.612, "y": 6681772.068, "z": 46.69800000000396}, {
        "x": 358640.03,
        "y": 6681782.117,
        "z": 45.845000000001164
      }, {"x": 358667.079, "y": 6681800.477, "z": 44.9320000000007}, {
        "x": 358689.272,
        "y": 6681816.32,
        "z": 45.47299999999814
      }, {"x": 358710.053, "y": 6681830.82, "z": 45.78100000000268}, {
        "x": 358728.051,
        "y": 6681843.321,
        "z": 46.46899999999732
      }, {"x": 358747.534, "y": 6681857.312, "z": 47.83999999999651}, {
        "x": 358761.797,
        "y": 6681868.403,
        "z": 49.09900000000198
      }, {"x": 358780.93, "y": 6681884.141, "z": 50.479000000006636}, {
        "x": 358795.703,
        "y": 6681898.219,
        "z": 50.74300000000221
      }, {"x": 358807.263, "y": 6681909.34, "z": 50.47500000000582}, {
        "x": 358815.197,
        "y": 6681918.848,
        "z": 50.00900000000547
      }, {"x": 358822.405, "y": 6681929.959, "z": 49.546000000002095}],
      "id": 14469,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2627,
      "endMValue": 287.65,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356064832,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "14.10.2016 18:15:13",
      "linkId": 1717211,
      "startAddressM": 5018,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 6052,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359509.691, "y": 6679711.92, "z": 52.763999999995576}, {
        "x": 359507.962,
        "y": 6679715.361,
        "z": 52.679999999993015
      }, {"x": 359496.087, "y": 6679738.282, "z": 52.56900000000314}, {
        "x": 359483.801,
        "y": 6679761.342,
        "z": 52.96899999999732
      }, {"x": 359477.8272161873, "y": 6679772.108610364, "z": 53.29398823888345}],
      "id": 6052,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5086,
      "endMValue": 68.107,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 362905935,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1716767,
      "startAddressM": 2954,
      "roadNameFi": "Lapinkyläntie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 53306,
      "municipalityCode": 257,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1131,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 358691.072, "y": 6681401.284, "z": 48.30000000000291}, {
        "x": 358685.842,
        "y": 6681405.063,
        "z": 48.37699999999313
      }, {"x": 358673.358, "y": 6681414.434, "z": 48.6359999999986}, {
        "x": 358660.356,
        "y": 6681425.504,
        "z": 48.96700000000419
      }, {"x": 358647.994, "y": 6681438.756, "z": 49.35199999999895}, {
        "x": 358638.628,
        "y": 6681450.854,
        "z": 49.64299999999639
      }, {"x": 358629.168, "y": 6681463.591, "z": 49.9320000000007}],
      "id": 53306,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3043,
      "endMValue": 88.427,
      "roadNameSe": "Lappbölevägen",
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 356063495,
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    }]];
  };

  var generateProjectLinkData = function () {
    return [[{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177060,
      "startAddressM": 4043,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 251890,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 537581.884, "y": 6989823.418, "z": 98.82700000000477}, {
        "x": 537603.533,
        "y": 6989835.0,
        "z": 98.30499999999302
      }, {"x": 537617.725, "y": 6989841.074, "z": 98.1710000000021}, {
        "x": 537630.174,
        "y": 6989847.03,
        "z": 98.09399999999732
      }, {"x": 537656.679, "y": 6989855.675, "z": 98.05800000000454}, {
        "x": 537682.441,
        "y": 6989861.523,
        "z": 98.30800000000454
      }, {"x": 537708.361, "y": 6989863.255, "z": 98.6359999999986}, {
        "x": 537733.974,
        "y": 6989861.597,
        "z": 98.9030000000057
      }, {"x": 537757.319, "y": 6989857.585, "z": 99.24000000000524}, {
        "x": 537785.649,
        "y": 6989848.573,
        "z": 99.51399999999558
      }, {"x": 537792.078, "y": 6989846.167, "z": 99.50900000000547}],
      "id": 251890,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4263,
      "endMValue": 220.012,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847946,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177056,
      "startAddressM": 4495,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 206601,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 537223.86, "y": 6989667.048, "z": 104.30199999999604}, {
        "x": 537241.668,
        "y": 6989668.707,
        "z": 104.2039999999979
      }, {"x": 537267.047, "y": 6989671.767, "z": 104.28100000000268}, {
        "x": 537287.197,
        "y": 6989677.121,
        "z": 104.34399999999732
      }, {"x": 537308.44, "y": 6989683.701, "z": 104.44100000000617}, {
        "x": 537334.621,
        "y": 6989694.08,
        "z": 104.62099999999919
      }, {"x": 537358.531, "y": 6989705.765, "z": 104.70799999999872}, {
        "x": 537376.8088097714,
        "y": 6989716.093892501,
        "z": 104.81399889680209
      }],
      "id": 206601,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4657,
      "endMValue": 162.306,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846140,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172223,
      "startAddressM": 4730,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 259613,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 537079.386, "y": 6989666.881, "z": 110.28800000000047}, {
        "x": 537096.482,
        "y": 6989668.765,
        "z": 109.48699999999371
      }, {"x": 537132.258, "y": 6989668.564, "z": 107.32600000000093}, {
        "x": 537150.503,
        "y": 6989668.216,
        "z": 106.40899999999965
      }, {"x": 537151.1619828065, "y": 6989668.187000756, "z": 106.38000075662258}],
      "id": 259613,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4802,
      "endMValue": 71.884,
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 537079.386, "y": 6989666.881, "z": 110.28800000000047}, "value": 4802}],
      "mmlId": 318846164,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177062,
      "startAddressM": 3958,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 219591,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 537861.34, "y": 6989796.947, "z": 98.69599999999627}, {
        "x": 537830.576,
        "y": 6989822.016,
        "z": 99.11100000000442
      }, {"x": 537809.81, "y": 6989836.462, "z": 99.42299999999523}, {
        "x": 537792.0783318857,
        "y": 6989846.166818354,
        "z": 99.50899839036344
      }],
      "id": 219591,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4043,
      "endMValue": 85.195,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846860,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177057,
      "startAddressM": 4263,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 205673,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 537410.956, "y": 6989734.488, "z": 104.80599999999686}, {
        "x": 537441.2,
        "y": 6989751.113,
        "z": 104.17900000000373
      }, {"x": 537478.555, "y": 6989771.156, "z": 102.76600000000326}, {
        "x": 537509.567,
        "y": 6989786.07,
        "z": 101.37099999999919
      }, {"x": 537545.008, "y": 6989804.09, "z": 100.01399999999558}, {
        "x": 537580.356,
        "y": 6989822.578,
        "z": 98.8579999999929
      }, {"x": 537581.8839194343, "y": 6989823.41795571, "z": 98.82700163451804}],
      "id": 205673,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4456,
      "endMValue": 192.71,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846488,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177064,
      "startAddressM": 4456,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 258351,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 537376.809, "y": 6989716.094, "z": 104.81399999999849}, {
        "x": 537392.003,
        "y": 6989724.079,
        "z": 104.99199999999837
      }, {"x": 537410.956, "y": 6989734.488, "z": 104.80599999999686}],
      "id": 258351,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4495,
      "endMValue": 38.788,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846134,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177149,
      "startAddressM": 3392,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 215352,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 538341.826, "y": 6989520.072, "z": 95.17600000000675}, {
        "x": 538341.272,
        "y": 6989520.243,
        "z": 95.17799999999988
      }, {"x": 538285.473, "y": 6989537.834, "z": 95.50100000000384}, {
        "x": 538248.473,
        "y": 6989548.795,
        "z": 95.69800000000396
      }, {"x": 538191.079, "y": 6989566.371, "z": 96.1710000000021}, {
        "x": 538152.479,
        "y": 6989579.503,
        "z": 96.35899999999674
      }],
      "id": 215352,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3591,
      "endMValue": 198.473,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 931719326,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177146,
      "startAddressM": 3591,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 191361,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 538152.479, "y": 6989579.503, "z": 96.35899999999674}, {
        "x": 538145.108,
        "y": 6989582.124,
        "z": 96.39400000000023
      }, {"x": 538103.947, "y": 6989598.906, "z": 96.61500000000524}, {
        "x": 538072.9961679971,
        "y": 6989615.277911135,
        "z": 96.87399859418505
      }],
      "id": 191361,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3678,
      "endMValue": 87.288,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847028,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5177148,
      "startAddressM": 3678,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 239931,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 538072.996, "y": 6989615.278, "z": 96.87399999999616}, {
        "x": 538060.546,
        "y": 6989622.078,
        "z": 96.92799999999988
      }, {"x": 538020.859, "y": 6989650.073, "z": 97.16099999999278}, {
        "x": 537986.258,
        "y": 6989681.139,
        "z": 97.35400000000664
      }, {"x": 537949.379, "y": 6989714.035, "z": 97.6030000000028}, {
        "x": 537913.111,
        "y": 6989748.515,
        "z": 97.96600000000035
      }, {"x": 537877.664, "y": 6989781.528, "z": 98.50500000000466}, {
        "x": 537861.562,
        "y": 6989796.748,
        "z": 98.69500000000698
      }, {"x": 537861.3401124008, "y": 6989796.946899244, "z": 98.6959994936861}],
      "id": 239931,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3958,
      "endMValue": 279.609,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847016,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172232,
      "startAddressM": 4657,
      "roadNameFi": "Siltasalmentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 228515,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16330,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 537223.86, "y": 6989667.048, "z": 104.30199999999604}, {
        "x": 537222.875,
        "y": 6989666.987,
        "z": 104.31699999999546
      }, {"x": 537208.491, "y": 6989666.702, "z": 104.52700000000186}, {
        "x": 537182.018,
        "y": 6989666.549,
        "z": 105.17600000000675
      }, {"x": 537151.162, "y": 6989668.187, "z": 106.38000000000466}],
      "id": 228515,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4730,
      "endMValue": 72.747,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846170,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171715,
      "startAddressM": 7863,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 213260,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536470.348, "y": 6990327.44, "z": 93.63499999999476}, {
        "x": 536431.6060500479,
        "y": 6990352.680967393,
        "z": 92.98900083452023
      }],
      "id": 213260,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7909,
      "endMValue": 46.239,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318845852,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172459,
      "startAddressM": 3592,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 454434,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536465.347, "y": 6986531.87, "z": 105.31200000000536}, {
        "x": 536468.022,
        "y": 6986542.254,
        "z": 105.23399999999674
      }, {"x": 536475.6468933235, "y": 6986568.344634977, "z": 105.04600263018959}],
      "id": 454434,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3630,
      "endMValue": 37.905,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318830571,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172405,
      "startAddressM": 4085,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 233697,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536600.568, "y": 6987006.183, "z": 99.65700000000652}, {
        "x": 536617.501,
        "y": 6987046.397,
        "z": 100.34100000000035
      }, {"x": 536628.192, "y": 6987072.013, "z": 101.00599999999395}, {
        "x": 536639.052,
        "y": 6987097.1,
        "z": 101.75
      }, {"x": 536652.479, "y": 6987124.121, "z": 102.74899999999616}, {
        "x": 536671.025,
        "y": 6987159.508,
        "z": 104.21099999999569
      }, {"x": 536687.656, "y": 6987189.751, "z": 105.26399999999558}, {
        "x": 536706.21,
        "y": 6987221.925,
        "z": 105.92600000000675
      }, {"x": 536715.517, "y": 6987238.847, "z": 106.08100000000559}],
      "id": 233697,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4345,
      "endMValue": 259.821,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848142,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172588,
      "startAddressM": 2787,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 233059,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536271.525, "y": 6985750.521, "z": 112.62799999999697}, {
        "x": 536278.043,
        "y": 6985776.319,
        "z": 113.00900000000547
      }, {"x": 536285.738, "y": 6985805.94, "z": 113.34200000000419}, {
        "x": 536319.017,
        "y": 6985938.469,
        "z": 113.97500000000582
      }, {"x": 536334.835, "y": 6986002.156, "z": 113.73600000000442}, {
        "x": 536342.908,
        "y": 6986034.555,
        "z": 113.48200000000361
      }, {"x": 536352.44, "y": 6986072.803, "z": 113.00100000000384}, {
        "x": 536355.575,
        "y": 6986085.175,
        "z": 112.82099999999627
      }],
      "id": 233059,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3132,
      "endMValue": 345.049,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 925446146,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174614,
      "startAddressM": 1435,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 247102,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535776.772, "y": 6984538.883, "z": 103.55000000000291}, {
        "x": 535777.15,
        "y": 6984539.106,
        "z": 103.54300000000512
      }, {"x": 535801.923, "y": 6984552.272, "z": 102.9780000000028}, {
        "x": 535822.7,
        "y": 6984563.341,
        "z": 102.55400000000373
      }, {"x": 535847.344820069, "y": 6984578.654888194, "z": 101.89700479669618}],
      "id": 247102,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1516,
      "endMValue": 81.05,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827753,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171707,
      "startAddressM": 8375,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 207554,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536043.327, "y": 6990610.009, "z": 94.70600000000559}, {
        "x": 536018.833,
        "y": 6990631.405,
        "z": 94.94599999999627
      }, {"x": 535943.612, "y": 6990693.375, "z": 96.46199999999953}, {
        "x": 535938.17329298,
        "y": 6990697.885757008,
        "z": 96.65798944215761
      }],
      "id": 207554,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8512,
      "endMValue": 137.049,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848082,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172461,
      "startAddressM": 3573,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 216008,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536460.817, "y": 6986514.105, "z": 105.39500000000407}, {
        "x": 536465.3468838496,
        "y": 6986531.8695445005,
        "z": 105.31200212814676
      }],
      "id": 216008,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3592,
      "endMValue": 18.333,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 345367507,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172625,
      "startAddressM": 2430,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 242160,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536185.989, "y": 6985404.387, "z": 101.90700000000652}, {
        "x": 536189.506,
        "y": 6985418.556,
        "z": 102.6079999999929
      }],
      "id": 242160,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2445,
      "endMValue": 14.599,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1096161792,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172291,
      "startAddressM": 5175,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 231255,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537128.115, "y": 6987959.092, "z": 107.34799999999814}, {
        "x": 537128.579,
        "y": 6987959.942,
        "z": 107.3179999999993
      }, {"x": 537145.749, "y": 6987989.457, "z": 105.55400000000373}, {
        "x": 537162.355,
        "y": 6988018.805,
        "z": 104.03699999999662
      }, {"x": 537173.771, "y": 6988039.555, "z": 102.90799999999581}, {
        "x": 537190.717,
        "y": 6988069.784,
        "z": 101.61800000000221
      }, {"x": 537194.482, "y": 6988076.726, "z": 101.35700000000361}, {
        "x": 537210.212,
        "y": 6988105.864,
        "z": 100.44199999999546
      }, {"x": 537227.616, "y": 6988143.697, "z": 99.46700000000419}, {
        "x": 537242.698,
        "y": 6988181.373,
        "z": 98.62200000000303
      }, {"x": 537258.058, "y": 6988228.308, "z": 97.6140000000014}, {
        "x": 537259.0168991017,
        "y": 6988231.765636177,
        "z": 97.56500515539135
      }],
      "id": 231255,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5478,
      "endMValue": 303.382,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 981752565,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "03.09.2016 22:29:28",
      "linkId": 499754523,
      "startAddressM": 4069,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 231800,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536595.073, "y": 6986991.014, "z": 99.52099999999336}, {
        "x": 536600.568,
        "y": 6987006.183,
        "z": 99.65700000000652
      }],
      "id": 231800,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4085,
      "endMValue": 16.134,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832491,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172499,
      "startAddressM": 3132,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 266691,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536355.575, "y": 6986085.175, "z": 112.82099999999627}, {
        "x": 536367.97,
        "y": 6986134.09,
        "z": 112.00800000000163
      }, {"x": 536378.5629787042, "y": 6986176.360915019, "z": 111.19800162839664}],
      "id": 266691,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3226,
      "endMValue": 94.039,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 925446149,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174592,
      "startAddressM": 1582,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 268420,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535900.773, "y": 6984618.321, "z": 99.91300000000047}, {
        "x": 535917.741,
        "y": 6984635.692,
        "z": 100.14800000000105
      }, {"x": 535929.213, "y": 6984648.108, "z": 99.8289999999979}],
      "id": 268420,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1623,
      "endMValue": 41.188,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832026,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172286,
      "startAddressM": 6048,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 193504,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537263.565, "y": 6988798.342, "z": 97.02999999999884}, {
        "x": 537263.532,
        "y": 6988798.684,
        "z": 97.03100000000268
      }, {"x": 537262.847, "y": 6988805.905, "z": 96.98799999999756}, {
        "x": 537262.746,
        "y": 6988807.35,
        "z": 96.98699999999371
      }],
      "id": 193504,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6057,
      "endMValue": 9.046,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846968,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172472,
      "startAddressM": 3522,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 241149,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536448.895, "y": 6986464.38, "z": 105.68600000000151}, {
        "x": 536449.122,
        "y": 6986465.378,
        "z": 105.66800000000512
      }, {"x": 536449.193, "y": 6986465.682, "z": 105.66199999999662}, {
        "x": 536451.531,
        "y": 6986475.675,
        "z": 105.57300000000396
      }],
      "id": 241149,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3534,
      "endMValue": 11.599,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 770664823,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174626,
      "startAddressM": 1418,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 236012,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535763.101, "y": 6984529.997, "z": 103.90200000000186}, {
        "x": 535767.49,
        "y": 6984532.686,
        "z": 103.81299999999464
      }, {"x": 535776.772, "y": 6984538.883, "z": 103.55000000000291}],
      "id": 236012,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1435,
      "endMValue": 16.308,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 697622490,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 6565548,
      "startAddressM": 6194,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 253036,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537254.538, "y": 6988944.06, "z": 96.42299999999523}, {
        "x": 537252.966,
        "y": 6988971.169,
        "z": 96.3469999999943
      }, {"x": 537250.734, "y": 6989007.118, "z": 96.8289999999979}, {
        "x": 537247.922,
        "y": 6989053.891,
        "z": 98.63300000000163
      }, {"x": 537243.8, "y": 6989110.51, "z": 101.8640000000014}, {
        "x": 537241.882,
        "y": 6989148.245,
        "z": 104.22500000000582
      }, {"x": 537240.083, "y": 6989171.419, "z": 105.82000000000698}, {
        "x": 537238.31,
        "y": 6989193.917,
        "z": 107.13499999999476
      }, {"x": 537236.486, "y": 6989221.001, "z": 108.49599999999919}],
      "id": 253036,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6471,
      "endMValue": 277.54,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 320507578,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "17.11.2016 23:00:21",
      "linkId": 499986843,
      "startAddressM": 4057,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 209635,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536590.84, "y": 6986979.391, "z": 99.44700000000012}, {
        "x": 536591.281,
        "y": 6986980.565,
        "z": 99.45200000000477
      }, {"x": 536594.866, "y": 6986990.444, "z": 99.53100000000268}, {
        "x": 536595.073,
        "y": 6986991.014,
        "z": 99.52099999999336
      }],
      "id": 209635,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4069,
      "endMValue": 12.37,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1733265907,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172284,
      "startAddressM": 5835,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 242260,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537274.977, "y": 6988586.164, "z": 98.63700000000244}, {
        "x": 537273.345,
        "y": 6988624.463,
        "z": 99.09100000000035
      }, {"x": 537269.841, "y": 6988685.231, "z": 98.64900000000489}, {
        "x": 537266.359,
        "y": 6988744.446,
        "z": 97.59200000000419
      }, {"x": 537263.5650183968, "y": 6988798.341645126, "z": 97.0300037004475}],
      "id": 242260,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6048,
      "endMValue": 212.488,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 931667290,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171736,
      "startAddressM": 8743,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 230910,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535760.325, "y": 6990846.562, "z": 98.81200000000536}, {
        "x": 535755.766,
        "y": 6990850.336,
        "z": 98.77400000000489
      }, {"x": 535738.377, "y": 6990864.187, "z": 98.77700000000186}, {
        "x": 535702.924,
        "y": 6990893.949,
        "z": 98.78399999999965
      }, {"x": 535665.531, "y": 6990926.227, "z": 98.88099999999395}, {
        "x": 535639.46,
        "y": 6990948.496,
        "z": 99.21600000000035
      }, {"x": 535623.526, "y": 6990962.775, "z": 99.51200000000244}, {
        "x": 535596.677,
        "y": 6990986.903,
        "z": 100.09200000000419
      }, {"x": 535548.214, "y": 6991034.928, "z": 100.61999999999534}, {
        "x": 535517.106,
        "y": 6991069.0,
        "z": 100.721000000005
      }, {"x": 535516.671, "y": 6991069.52, "z": 100.71300000000338}],
      "id": 230910,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 9074,
      "endMValue": 330.66,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1018688968,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174594,
      "startAddressM": 1623,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 231117,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535929.213, "y": 6984648.108, "z": 99.8289999999979}, {
        "x": 535929.63,
        "y": 6984648.55,
        "z": 99.81699999999546
      }, {"x": 535952.567, "y": 6984674.839, "z": 99.0570000000007}, {
        "x": 535976.491,
        "y": 6984705.616,
        "z": 98.16899999999441
      }, {"x": 535993.505, "y": 6984731.465, "z": 97.35400000000664}, {
        "x": 536012.207,
        "y": 6984764.768,
        "z": 96.21499999999651
      }, {"x": 536028.4728668111, "y": 6984800.463707714, "z": 95.01200985038473}],
      "id": 231117,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1806,
      "endMValue": 182.846,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832032,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172374,
      "startAddressM": 4345,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 243176,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536715.517, "y": 6987238.847, "z": 106.08100000000559}, {
        "x": 536734.504,
        "y": 6987271.771,
        "z": 106.09799999999814
      }, {"x": 536756.058, "y": 6987308.69, "z": 105.65600000000268}, {
        "x": 536775.468,
        "y": 6987341.654,
        "z": 104.69100000000617
      }, {"x": 536796.518, "y": 6987378.105, "z": 103.26399999999558}, {
        "x": 536823.089,
        "y": 6987423.877,
        "z": 101.94999999999709
      }, {"x": 536840.699, "y": 6987454.586, "z": 102.22299999999814}, {
        "x": 536843.4278589145,
        "y": 6987460.789679262,
        "z": 102.34999343428588
      }],
      "id": 243176,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4601,
      "endMValue": 256.206,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848136,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172227,
      "startAddressM": 6946,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 235119,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537079.386, "y": 6989666.881, "z": 110.28800000000047}, {
        "x": 537065.235,
        "y": 6989699.347,
        "z": 108.26499999999942
      }, {"x": 537041.462, "y": 6989748.966, "z": 109.34200000000419}, {
        "x": 537025.911,
        "y": 6989784.225,
        "z": 108.99899999999616
      }, {"x": 537025.5690551063, "y": 6989784.955882215, "z": 108.9860020946856}],
      "id": 235119,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7075,
      "endMValue": 129.779,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846152,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172299,
      "startAddressM": 5478,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 242102,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537259.017, "y": 6988231.766, "z": 97.56500000000233}, {
        "x": 537267.848,
        "y": 6988264.26,
        "z": 97.00199999999313
      }, {"x": 537268.029, "y": 6988264.972, "z": 96.99800000000687}, {
        "x": 537268.2099392074,
        "y": 6988265.673764219,
        "z": 96.99500100760744
      }],
      "id": 242102,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5513,
      "endMValue": 35.132,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846452,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172605,
      "startAddressM": 2445,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 214907,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536189.506, "y": 6985418.556, "z": 102.6079999999929}, {
        "x": 536215.622,
        "y": 6985522.092,
        "z": 106.82399999999325
      }, {"x": 536229.4369731247, "y": 6985577.707891806, "z": 108.61199652167662}],
      "id": 214907,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2609,
      "endMValue": 164.085,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023876448,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171702,
      "startAddressM": 7909,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 236635,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536431.606, "y": 6990352.681, "z": 92.9890000000014}, {
        "x": 536430.904,
        "y": 6990353.139,
        "z": 93.00500000000466
      }, {"x": 536358.356, "y": 6990400.249, "z": 92.96400000000722}, {
        "x": 536310.858267153,
        "y": 6990431.491824273,
        "z": 92.84300068055934
      }],
      "id": 236635,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8053,
      "endMValue": 144.192,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1018698063,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172477,
      "startAddressM": 3449,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 239254,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536432.155, "y": 6986392.926, "z": 106.58400000000256}, {
        "x": 536442.438,
        "y": 6986435.928,
        "z": 105.98200000000361
      }, {"x": 536448.895, "y": 6986464.38, "z": 105.68600000000151}],
      "id": 239254,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3522,
      "endMValue": 73.39,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 870187694,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172387,
      "startAddressM": 4924,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 263638,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537005.386, "y": 6987740.234, "z": 113.89400000000023}, {
        "x": 537016.076,
        "y": 6987760.501,
        "z": 113.71899999999732
      }, {"x": 537022.401, "y": 6987772.038, "z": 113.59399999999732}],
      "id": 263638,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4960,
      "endMValue": 36.071,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848238,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172487,
      "startAddressM": 3274,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 240033,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536390.374, "y": 6986223.488, "z": 110.09500000000116}, {
        "x": 536395.415,
        "y": 6986243.827,
        "z": 109.59900000000198
      }, {"x": 536403.036, "y": 6986274.843, "z": 108.9429999999993}, {
        "x": 536408.63,
        "y": 6986298.477,
        "z": 108.46899999999732
      }, {"x": 536414.3649858193, "y": 6986320.511945515, "z": 107.97100123138618}],
      "id": 240033,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3374,
      "endMValue": 99.949,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 925446129,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "17.11.2016 23:00:21",
      "linkId": 499986828,
      "startAddressM": 3630,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 253189,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536475.647, "y": 6986568.345, "z": 105.0460000000021}, {
        "x": 536487.446,
        "y": 6986613.65,
        "z": 104.7329999999929
      }, {"x": 536499.386, "y": 6986660.57, "z": 104.16999999999825}, {
        "x": 536509.096,
        "y": 6986701.702,
        "z": 103.59500000000116
      }, {"x": 536517.376, "y": 6986732.019, "z": 103.096000000005}, {
        "x": 536530.15,
        "y": 6986780.602,
        "z": 102.03200000000652
      }, {"x": 536542.455, "y": 6986826.323, "z": 101.04499999999825}, {
        "x": 536554.927,
        "y": 6986869.466,
        "z": 100.16700000000128
      }, {"x": 536556.995, "y": 6986876.746, "z": 100.03500000000349}, {
        "x": 536570.679,
        "y": 6986922.364,
        "z": 99.46700000000419
      }, {"x": 536585.889, "y": 6986966.303, "z": 99.40899999999965}, {
        "x": 536590.84,
        "y": 6986979.391,
        "z": 99.44700000000012
      }],
      "id": 253189,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4057,
      "endMValue": 427.098,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1766503173,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172485,
      "startAddressM": 3374,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 191904,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536414.365, "y": 6986320.512, "z": 107.971000000005}, {
        "x": 536430.248,
        "y": 6986384.953,
        "z": 106.70699999999488
      }, {"x": 536432.1549067228, "y": 6986392.925610016, "z": 106.58400601630703}],
      "id": 191904,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3449,
      "endMValue": 74.567,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 770664767,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172500,
      "startAddressM": 3226,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 203993,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536378.563, "y": 6986176.361, "z": 111.19800000000396}, {
        "x": 536380.736,
        "y": 6986186.358,
        "z": 110.97400000000198
      }, {"x": 536384.785, "y": 6986200.632, "z": 110.6469999999972}, {
        "x": 536387.839,
        "y": 6986213.927,
        "z": 110.30000000000291
      }, {"x": 536390.3739428384, "y": 6986223.487784409, "z": 110.0950046225374}],
      "id": 203993,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3274,
      "endMValue": 48.6,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 925446126,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172616,
      "startAddressM": 2609,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 223880,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536229.437, "y": 6985577.708, "z": 108.61199999999371}, {
        "x": 536231.157,
        "y": 6985584.637,
        "z": 108.83999999999651
      }, {"x": 536247.374, "y": 6985651.987, "z": 110.63700000000244}, {
        "x": 536250.584,
        "y": 6985664.848,
        "z": 110.9429999999993
      }],
      "id": 223880,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2699,
      "endMValue": 89.67,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023876510,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171704,
      "startAddressM": 8192,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 188054,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536193.468, "y": 6990506.633, "z": 92.9210000000021}, {
        "x": 536160.255,
        "y": 6990528.88,
        "z": 93.15099999999802
      }, {"x": 536143.279, "y": 6990540.227, "z": 93.3640000000014}, {
        "x": 536093.997,
        "y": 6990573.169,
        "z": 94.0399999999936
      }, {"x": 536049.746, "y": 6990605.376, "z": 94.67500000000291}, {
        "x": 536049.303236083,
        "y": 6990605.72181561,
        "z": 94.68299573665344
      }],
      "id": 188054,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8367,
      "endMValue": 174.965,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318845876,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174714,
      "startAddressM": 1084,
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 201872,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535860.097, "y": 6984226.803, "z": 106.62200000000303}, {
        "x": 535865.933,
        "y": 6984264.68,
        "z": 105.86699999999837
      }, {"x": 535868.225, "y": 6984316.187, "z": 105.30000000000291}, {
        "x": 535864.365,
        "y": 6984361.107,
        "z": 105.50699999999779
      }, {"x": 535853.067, "y": 6984402.083, "z": 105.55599999999686}, {
        "x": 535837.496,
        "y": 6984432.459,
        "z": 105.42200000000594
      }, {"x": 535815.795, "y": 6984462.514, "z": 105.05199999999604}, {
        "x": 535795.254,
        "y": 6984488.159,
        "z": 104.61299999999756
      }, {"x": 535781.167, "y": 6984505.534, "z": 104.3350000000064}, {
        "x": 535771.773,
        "y": 6984518.676,
        "z": 104.11500000000524
      }, {"x": 535771.6120362739, "y": 6984518.916945702, "z": 104.11400022530556}],
      "id": 201872,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1404,
      "endMValue": 320.347,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318830970,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174625,
      "startAddressM": 1404,
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 213760,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535771.612, "y": 6984518.917, "z": 104.1140000000014}, {
        "x": 535763.101,
        "y": 6984529.997,
        "z": 103.90200000000186
      }],
      "id": 213760,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1418,
      "endMValue": 13.972,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318830976,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172563,
      "startAddressM": 2699,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 265258,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536250.584, "y": 6985664.848, "z": 110.9429999999993}, {
        "x": 536265.011,
        "y": 6985722.65,
        "z": 112.13199999999779
      }, {"x": 536271.5249211162, "y": 6985750.520662485, "z": 112.62799399348697}],
      "id": 265258,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2787,
      "endMValue": 88.197,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827879,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172295,
      "startAddressM": 5639,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 256793,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537284.88, "y": 6988390.425, "z": 95.33299999999872}, {
        "x": 537284.901,
        "y": 6988391.14,
        "z": 95.3289999999979
      }, {"x": 537284.676, "y": 6988408.099, "z": 95.20699999999488}, {
        "x": 537282.905,
        "y": 6988452.736,
        "z": 95.22900000000664
      }, {"x": 537279.024, "y": 6988524.118, "z": 96.86999999999534}, {
        "x": 537278.998007788,
        "y": 6988524.548870899,
        "z": 96.88199640553576
      }],
      "id": 256793,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5774,
      "endMValue": 134.267,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846206,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172385,
      "startAddressM": 4960,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 189292,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537022.401, "y": 6987772.038, "z": 113.59399999999732}, {
        "x": 537023.112,
        "y": 6987773.338,
        "z": 113.55899999999383
      }, {"x": 537023.484, "y": 6987773.986, "z": 113.53800000000047}, {
        "x": 537040.064,
        "y": 6987802.343,
        "z": 113.13300000000163
      }, {"x": 537063.641, "y": 6987844.041, "z": 112.30800000000454}, {
        "x": 537090.965,
        "y": 6987892.545,
        "z": 110.71099999999569
      }, {"x": 537110.288, "y": 6987926.436, "z": 109.12600000000384}, {
        "x": 537128.115,
        "y": 6987959.092,
        "z": 107.34799999999814
      }],
      "id": 189292,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5175,
      "endMValue": 214.868,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847964,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172224,
      "startAddressM": 6745,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 201920,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537157.315, "y": 6989481.847, "z": 112.92999999999302}, {
        "x": 537144.168,
        "y": 6989512.617,
        "z": 113.05899999999383
      }, {"x": 537126.718, "y": 6989553.824, "z": 112.74199999999837}, {
        "x": 537113.725,
        "y": 6989584.143,
        "z": 112.17399999999907
      }, {"x": 537096.822, "y": 6989625.289, "z": 111.29899999999907}, {
        "x": 537079.834,
        "y": 6989665.806,
        "z": 110.32399999999325
      }, {"x": 537079.386, "y": 6989666.881, "z": 110.28800000000047}],
      "id": 201920,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6946,
      "endMValue": 200.778,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846128,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171753,
      "startAddressM": 8512,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 211959,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535938.173, "y": 6990697.886, "z": 96.65799999999581}, {
        "x": 535861.234,
        "y": 6990761.681,
        "z": 98.5109999999986
      }, {"x": 535824.579, "y": 6990792.273, "z": 98.88800000000629}, {
        "x": 535787.067,
        "y": 6990823.463,
        "z": 98.91599999999744
      }, {"x": 535760.325, "y": 6990846.562, "z": 98.81200000000536}],
      "id": 211959,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8743,
      "endMValue": 231.813,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847616,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171708,
      "startAddressM": 8367,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 224166,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536049.303, "y": 6990605.722, "z": 94.68300000000454}, {
        "x": 536043.327,
        "y": 6990610.009,
        "z": 94.70600000000559
      }],
      "id": 224166,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8375,
      "endMValue": 7.355,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848088,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172294,
      "startAddressM": 5513,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 260873,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537268.21, "y": 6988265.674, "z": 96.99499999999534}, {
        "x": 537274.664,
        "y": 6988293.625,
        "z": 96.5219999999972
      }, {"x": 537280.981, "y": 6988335.867, "z": 95.70699999999488}, {
        "x": 537284.053,
        "y": 6988366.193,
        "z": 95.45900000000256
      }, {"x": 537284.879983676, "y": 6988390.424521688, "z": 95.33300248709313}],
      "id": 260873,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5639,
      "endMValue": 126.125,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 982300200,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172395,
      "startAddressM": 4601,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 252887,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536843.428, "y": 6987460.79, "z": 102.35000000000582}, {
        "x": 536859.608,
        "y": 6987486.401,
        "z": 103.29399999999441
      }, {"x": 536873.859, "y": 6987512.303, "z": 104.48399999999674}, {
        "x": 536884.975,
        "y": 6987531.113,
        "z": 105.68099999999686
      }, {"x": 536905.14, "y": 6987565.515, "z": 108.02599999999802}, {
        "x": 536921.93,
        "y": 6987595.148,
        "z": 109.96799999999348
      }, {"x": 536922.963, "y": 6987596.958, "z": 110.07300000000396}],
      "id": 252887,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4759,
      "endMValue": 157.726,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847916,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172216,
      "startAddressM": 6471,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 212228,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537236.486, "y": 6989221.001, "z": 108.49599999999919}, {
        "x": 537236.379,
        "y": 6989222.594,
        "z": 108.56600000000617
      }, {"x": 537233.732, "y": 6989246.77, "z": 109.49599999999919}, {
        "x": 537227.312,
        "y": 6989280.057,
        "z": 110.32499999999709
      }, {"x": 537222.362, "y": 6989300.666, "z": 110.64800000000105}, {
        "x": 537217.106,
        "y": 6989321.169,
        "z": 110.87099999999919
      }, {"x": 537207.198, "y": 6989353.368, "z": 111.12900000000081}, {
        "x": 537191.142,
        "y": 6989399.58,
        "z": 111.53900000000431
      }, {"x": 537174.616, "y": 6989441.899, "z": 112.19899999999325}, {
        "x": 537157.548,
        "y": 6989481.303,
        "z": 112.91999999999825
      }, {"x": 537157.315091002, "y": 6989481.846787532, "z": 112.92999609432304}],
      "id": 212228,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6745,
      "endMValue": 273.754,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846590,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171692,
      "startAddressM": 7482,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 231594,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536773.025, "y": 6990097.884, "z": 98.9890000000014}, {
        "x": 536762.828,
        "y": 6990106.418,
        "z": 99.02499999999418
      }, {"x": 536735.567, "y": 6990128.664, "z": 99.15099999999802}, {
        "x": 536706.322,
        "y": 6990153.372,
        "z": 99.46899999999732
      }, {"x": 536659.996, "y": 6990191.916, "z": 98.69800000000396}, {
        "x": 536639.254,
        "y": 6990209.115,
        "z": 98.0109999999986
      }, {"x": 536620.154, "y": 6990224.336, "z": 97.36000000000058}, {
        "x": 536601.324,
        "y": 6990239.004,
        "z": 96.78900000000431
      }, {"x": 536572.46, "y": 6990260.28, "z": 96.04300000000512}, {
        "x": 536541.366,
        "y": 6990281.482,
        "z": 95.38800000000629
      }, {"x": 536507.56, "y": 6990303.717, "z": 94.55899999999383}, {
        "x": 536470.348,
        "y": 6990327.44,
        "z": 93.63499999999476
      }],
      "id": 231594,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7863,
      "endMValue": 380.355,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847412,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174496,
      "startAddressM": 1906,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 454435,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536058.172, "y": 6984896.208, "z": 91.38800000000629}, {
        "x": 536074.586,
        "y": 6984959.893,
        "z": 89.36900000000605
      }, {"x": 536091.757, "y": 6985028.584, "z": 88.79300000000512}, {
        "x": 536147.567,
        "y": 6985250.652,
        "z": 94.83199999999488
      }, {"x": 536178.613, "y": 6985374.433, "z": 100.65099999999802}, {
        "x": 536185.9889125096,
        "y": 6985404.3866447015,
        "z": 101.90698510198757
      }],
      "id": 454435,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2430,
      "endMValue": 524.008,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1096161804,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172399,
      "startAddressM": 4759,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 207551,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536922.963, "y": 6987596.958, "z": 110.07300000000396}, {
        "x": 536930.1707634986,
        "y": 6987609.584585696,
        "z": 110.76397732762476
      }],
      "id": 207551,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4773,
      "endMValue": 14.539,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846464,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171703,
      "startAddressM": 8053,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 212181,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536310.858, "y": 6990431.492, "z": 92.84299999999348}, {
        "x": 536309.905,
        "y": 6990432.118,
        "z": 92.8530000000028
      }, {"x": 536294.239, "y": 6990442.092, "z": 92.79700000000594}, {
        "x": 536223.037,
        "y": 6990487.763,
        "z": 92.71300000000338
      }, {"x": 536193.4684039024, "y": 6990506.632742243, "z": 92.92099715879388}],
      "id": 212181,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 8192,
      "endMValue": 139.379,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 982443614,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172210,
      "startAddressM": 7075,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 191763,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537025.569, "y": 6989784.956, "z": 108.98600000000442}, {
        "x": 537020.815,
        "y": 6989794.739,
        "z": 108.81900000000314
      }, {"x": 537006.39, "y": 6989824.582, "z": 107.91099999999278}, {
        "x": 536984.529,
        "y": 6989866.575,
        "z": 106.3640000000014
      }, {"x": 536964.639, "y": 6989898.811, "z": 105.25400000000081}, {
        "x": 536941.306,
        "y": 6989932.727,
        "z": 104.09799999999814
      }, {"x": 536920.115, "y": 6989959.333, "z": 103.00400000000081}, {
        "x": 536892.589,
        "y": 6989990.807,
        "z": 101.42399999999907
      }, {"x": 536861.719, "y": 6990022.163, "z": 99.9429999999993}, {
        "x": 536833.194,
        "y": 6990047.598,
        "z": 99.23799999999756
      }, {"x": 536798.715, "y": 6990076.38, "z": 98.9600000000064}, {
        "x": 536773.025,
        "y": 6990097.884,
        "z": 98.9890000000014
      }],
      "id": 191763,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 7482,
      "endMValue": 406.873,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318847418,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174554,
      "startAddressM": 1806,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 266082,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536028.473, "y": 6984800.464, "z": 95.01200000000244}, {
        "x": 536029.318,
        "y": 6984802.536,
        "z": 94.95900000000256
      }, {"x": 536044.742, "y": 6984844.507, "z": 93.37300000000687}, {
        "x": 536058.172,
        "y": 6984896.208,
        "z": 91.38800000000629
      }],
      "id": 266082,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1906,
      "endMValue": 100.37,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1350522745,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172455,
      "startAddressM": 3534,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 229270,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536451.531, "y": 6986475.675, "z": 105.57300000000396}, {
        "x": 536457.326,
        "y": 6986500.413,
        "z": 105.44500000000698
      }, {"x": 536460.817, "y": 6986514.105, "z": 105.39500000000407}],
      "id": 229270,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3573,
      "endMValue": 39.538,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 345367528,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172383,
      "startAddressM": 4773,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 224651,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 536930.171, "y": 6987609.585, "z": 110.76399999999558}, {
        "x": 536935.632,
        "y": 6987619.153,
        "z": 111.24300000000221
      }, {"x": 536952.008, "y": 6987647.213, "z": 112.3289999999979}, {
        "x": 536978.387,
        "y": 6987692.673,
        "z": 113.52599999999802
      }, {"x": 536994.508, "y": 6987721.573, "z": 113.88499999999476}, {
        "x": 537005.3858650307,
        "y": 6987740.233768463,
        "z": 113.89399988833229
      }],
      "id": 224651,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4924,
      "endMValue": 150.757,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318848100,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174619,
      "startAddressM": 1516,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 194364,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 535847.345, "y": 6984578.655, "z": 101.8969999999972}, {
        "x": 535865.152,
        "y": 6984591.565,
        "z": 101.24099999999453
      }, {"x": 535883.775, "y": 6984606.153, "z": 100.53599999999278}, {
        "x": 535897.135,
        "y": 6984615.641,
        "z": 100.04700000000594
      }, {"x": 535900.773, "y": 6984618.321, "z": 99.91300000000047}],
      "id": 194364,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1582,
      "endMValue": 66.556,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827711,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172269,
      "startAddressM": 6057,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 188088,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537262.746, "y": 6988807.35, "z": 96.98699999999371}, {
        "x": 537259.766,
        "y": 6988850.014,
        "z": 96.75800000000163
      }, {"x": 537256.496, "y": 6988903.277, "z": 96.58000000000175}, {
        "x": 537254.5380098738,
        "y": 6988944.059794337,
        "z": 96.42300079172338
      }],
      "id": 188088,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6194,
      "endMValue": 136.961,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846398,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172287,
      "startAddressM": 5774,
      "roadNameFi": "Viitonen",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 225445,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 559,
      "trackCode": 0,
      "roadClass": 3,
      "sideCode": 2,
      "points": [{"x": 537278.998, "y": 6988524.549, "z": 96.88199999999779}, {
        "x": 537276.87,
        "y": 6988558.507,
        "z": 97.89400000000023
      }, {"x": 537274.9770217818, "y": 6988586.163681764, "z": 98.63699145064962}],
      "id": 225445,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5835,
      "endMValue": 61.746,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318846974,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172050,
      "startAddressM": 1939,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454550,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533232.769, "y": 6989604.369, "z": 112.89100000000326}, {
        "x": 533236.731,
        "y": 6989628.623,
        "z": 113.10700000000361
      }],
      "id": 454550,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1964,
      "endMValue": 24.575474766260065,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833222,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172090,
      "startAddressM": 738,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454546,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533333.28, "y": 6988419.385, "z": 112.2219999999943}, {
        "x": 533329.13,
        "y": 6988435.803,
        "z": 112.38300000000163
      }, {"x": 533312.863, "y": 6988500.586, "z": 112.98799999999756}, {
        "x": 533311.531,
        "y": 6988505.939,
        "z": 113.03900000000431
      }, {"x": 533309.26, "y": 6988518.355, "z": 113.12200000000303}, {
        "x": 533299.907,
        "y": 6988559.453,
        "z": 113.41300000000047
      }, {"x": 533288.322, "y": 6988615.82, "z": 113.64100000000326}, {
        "x": 533276.139,
        "y": 6988676.913,
        "z": 113.70100000000093
      }, {"x": 533264.419, "y": 6988743.886, "z": 113.56100000000151}, {
        "x": 533255.948,
        "y": 6988797.772,
        "z": 113.24599999999919
      }, {"x": 533246.406, "y": 6988863.807, "z": 112.68799999999464}, {
        "x": 533236.578,
        "y": 6988934.662,
        "z": 112.0219999999972
      }, {"x": 533229.882, "y": 6988997.097, "z": 111.4320000000007}, {
        "x": 533227.18,
        "y": 6989021.018,
        "z": 111.2329999999929
      }],
      "id": 454546,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1352,
      "endMValue": 611.5155153400723,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834506,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171957,
      "startAddressM": 2767,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454553,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533474.305, "y": 6990390.877, "z": 116.76300000000629}, {
        "x": 533477.083,
        "y": 6990400.196,
        "z": 116.74499999999534
      }, {"x": 533481.414, "y": 6990422.003, "z": 116.61000000000058}, {
        "x": 533490.536,
        "y": 6990467.799,
        "z": 116.21499999999651
      }, {"x": 533499.641, "y": 6990521.822, "z": 115.73500000000058}, {
        "x": 533506.9,
        "y": 6990573.462,
        "z": 115.2329999999929
      }, {"x": 533511.479, "y": 6990616.338, "z": 114.72500000000582}, {
        "x": 533516.668,
        "y": 6990667.467,
        "z": 114.17500000000291
      }, {"x": 533520.224, "y": 6990713.017, "z": 113.59399999999732}, {
        "x": 533523.687,
        "y": 6990762.188,
        "z": 113.00500000000466
      }, {"x": 533528.015, "y": 6990815.44, "z": 112.3640000000014}, {
        "x": 533532.327,
        "y": 6990862.766,
        "z": 111.80199999999604
      }, {"x": 533536.428, "y": 6990900.74, "z": 111.35199999999895}, {
        "x": 533542.931,
        "y": 6990941.121,
        "z": 110.87600000000384
      }, {"x": 533549.524, "y": 6990979.484, "z": 110.42399999999907}, {
        "x": 533556.546,
        "y": 6991016.053,
        "z": 110.03699999999662
      }, {"x": 533564.937, "y": 6991051.637, "z": 109.61299999999756}, {
        "x": 533569.148,
        "y": 6991068.307,
        "z": 109.23699999999371
      }],
      "id": 454553,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3455,
      "endMValue": 685.0400566860739,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833390,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172128,
      "startAddressM": 700,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454545,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533341.472, "y": 6988382.846, "z": 111.92299999999523}, {
        "x": 533333.28,
        "y": 6988419.385,
        "z": 112.2219999999943
      }],
      "id": 454545,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 738,
      "endMValue": 37.44605967240716,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1645159358,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172126,
      "startAddressM": 581,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454543,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533375.053, "y": 6988268.755, "z": 110.69700000000012}, {
        "x": 533371.369,
        "y": 6988280.73,
        "z": 110.80100000000675
      }, {"x": 533347.708, "y": 6988359.985, "z": 111.10899999999674}],
      "id": 454543,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 677,
      "endMValue": 95.24040045101987,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834482,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172041,
      "startAddressM": 1964,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454551,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533236.731, "y": 6989628.623, "z": 113.10700000000361}, {
        "x": 533247.374,
        "y": 6989681.903,
        "z": 113.60700000000361
      }, {"x": 533264.562, "y": 6989756.204, "z": 114.32499999999709}, {
        "x": 533281.826,
        "y": 6989817.599,
        "z": 114.9539999999979
      }, {"x": 533286.463, "y": 6989834.9, "z": 115.19000000000233}, {
        "x": 533301.249,
        "y": 6989880.321,
        "z": 115.66999999999825
      }, {"x": 533318.431, "y": 6989926.754, "z": 116.06600000000617}, {
        "x": 533335.282,
        "y": 6989971.221,
        "z": 116.41899999999441
      }, {"x": 533351.944, "y": 6990013.657, "z": 116.7219999999943}, {
        "x": 533382.213,
        "y": 6990090.374,
        "z": 117.06399999999849
      }, {"x": 533396.269, "y": 6990127.117, "z": 117.20799999999872}, {
        "x": 533415.894,
        "y": 6990181.329,
        "z": 117.29300000000512
      }, {"x": 533430.433, "y": 6990226.041, "z": 117.27999999999884}, {
        "x": 533443.281,
        "y": 6990268.028,
        "z": 117.25800000000163
      }, {"x": 533455.595, "y": 6990312.572, "z": 117.20699999999488}, {
        "x": 533458.656,
        "y": 6990324.877,
        "z": 117.13300000000163
      }],
      "id": 454551,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2699,
      "endMValue": 731.9903553295312,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833930,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172030,
      "startAddressM": 21,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454542,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533582.917, "y": 6987752.66, "z": 111.83400000000256}, {
        "x": 533581.069,
        "y": 6987755.632,
        "z": 111.75199999999313
      }, {"x": 533539.9, "y": 6987829.064, "z": 110.14100000000326}, {
        "x": 533513.471,
        "y": 6987883.543,
        "z": 109.29700000000594
      }, {"x": 533480.619, "y": 6987961.003, "z": 108.65099999999802}, {
        "x": 533445.937,
        "y": 6988052.528,
        "z": 108.68899999999849
      }, {"x": 533414.716, "y": 6988142.857, "z": 109.346000000005}, {
        "x": 533392.488,
        "y": 6988212.085,
        "z": 110.08800000000338
      }, {"x": 533375.053, "y": 6988268.755, "z": 110.69700000000012}],
      "id": 454542,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 581,
      "endMValue": 557.823278018479,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834464,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172012,
      "startAddressM": 2699,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454552,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533458.656, "y": 6990324.877, "z": 117.13300000000163}, {
        "x": 533460.015,
        "y": 6990331.892,
        "z": 117.09399999999732
      }, {"x": 533466.527, "y": 6990355.312, "z": 116.96099999999569}, {
        "x": 533470.139,
        "y": 6990371.299,
        "z": 116.87200000000303
      }, {"x": 533474.305, "y": 6990390.877, "z": 116.76300000000629}],
      "id": 454552,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2767,
      "endMValue": 67.86020462454705,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833396,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172038,
      "startAddressM": 1376,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454549,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533225.167, "y": 6989045.105, "z": 110.96499999999651}, {
        "x": 533219.973,
        "y": 6989110.791,
        "z": 110.35700000000361
      }, {"x": 533214.4, "y": 6989206.376, "z": 110.00599999999395}, {
        "x": 533211.057,
        "y": 6989286.772,
        "z": 110.125
      }, {"x": 533211.192, "y": 6989356.15, "z": 110.45900000000256}, {
        "x": 533213.14,
        "y": 6989423.864,
        "z": 111.04499999999825
      }, {"x": 533220.698, "y": 6989512.326, "z": 111.91000000000349}, {
        "x": 533229.614,
        "y": 6989581.92,
        "z": 112.58999999999651
      }, {"x": 533232.769, "y": 6989604.369, "z": 112.89100000000326}],
      "id": 454549,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1939,
      "endMValue": 560.8406903505286,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833228,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172714,
      "startAddressM": 0,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454541,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533593.916, "y": 6987734.971, "z": 112.37699999999313}, {
        "x": 533582.917,
        "y": 6987752.66,
        "z": 111.83400000000256
      }],
      "id": 454541,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 21,
      "endMValue": 20.829755687661187,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1026507682,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172134,
      "startAddressM": 677,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454544,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533347.708, "y": 6988359.985, "z": 111.10899999999674}, {
        "x": 533341.472,
        "y": 6988382.846,
        "z": 111.92299999999523
      }],
      "id": 454544,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 700,
      "endMValue": 23.696265886857788,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834488,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172100,
      "startAddressM": 1352,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454548,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533227.18, "y": 6989021.018, "z": 111.2329999999929}, {
        "x": 533225.167,
        "y": 6989045.105,
        "z": 110.96499999999651
      }],
      "id": 454548,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1376,
      "endMValue": 24.170968909295723,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833234,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174611,
      "startAddressM": 1909,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 268021,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535873.807, "y": 6984606.506, "z": 94.74800000000687}, {
        "x": 535872.863,
        "y": 6984611.307,
        "z": 94.64100000000326
      }, {"x": 535861.488, "y": 6984658.531, "z": 93.58599999999569}, {
        "x": 535844.733,
        "y": 6984714.176,
        "z": 92.26200000000244
      }, {"x": 535829.026, "y": 6984760.55, "z": 91.32300000000396}, {
        "x": 535812.225,
        "y": 6984803.153,
        "z": 90.54499999999825
      }, {"x": 535786.777, "y": 6984860.647, "z": 89.62900000000081}, {
        "x": 535760.264,
        "y": 6984915.968,
        "z": 88.97299999999814
      }, {"x": 535722.261, "y": 6984987.233, "z": 88.72000000000116}, {
        "x": 535686.606,
        "y": 6985048.56,
        "z": 88.7329999999929
      }, {"x": 535646.723, "y": 6985110.799, "z": 89.16000000000349}, {
        "x": 535604.0721567502,
        "y": 6985172.360773748,
        "z": 89.88599733181654
      }],
      "id": 268021,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2540,
      "endMValue": 631.076,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827777,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172034,
      "startAddressM": 3104,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 245259,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535204.948, "y": 6985567.333, "z": 105.59299999999348}, {
        "x": 535188.604,
        "y": 6985579.302,
        "z": 106.31600000000617
      }, {"x": 535157.351, "y": 6985600.684, "z": 107.59799999999814}, {
        "x": 535106.952,
        "y": 6985633.857,
        "z": 109.6030000000028
      }, {"x": 535051.721, "y": 6985667.523, "z": 111.77300000000105}, {
        "x": 534992.302,
        "y": 6985704.27,
        "z": 114.03599999999278
      }, {"x": 534932.116, "y": 6985742.104, "z": 116.05599999999686}, {
        "x": 534884.74,
        "y": 6985775.007,
        "z": 117.51900000000023
      }, {"x": 534838.037, "y": 6985812.144, "z": 118.70699999999488}, {
        "x": 534814.495,
        "y": 6985832.668,
        "z": 119.2329999999929
      }, {"x": 534804.664, "y": 6985841.504, "z": 119.44899999999325}, {
        "x": 534774.366,
        "y": 6985869.472,
        "z": 120.04700000000594
      }, {"x": 534746.615, "y": 6985897.252, "z": 120.51600000000326}, {
        "x": 534716.944,
        "y": 6985929.573,
        "z": 121.04499999999825
      }, {"x": 534685.503, "y": 6985965.598, "z": 121.65099999999802}, {
        "x": 534649.566,
        "y": 6986013.376,
        "z": 122.35599999999977
      }, {"x": 534620.672, "y": 6986054.529, "z": 122.97900000000664}, {
        "x": 534583.549,
        "y": 6986115.382,
        "z": 123.89999999999418
      }, {"x": 534559.29, "y": 6986160.417, "z": 124.70200000000477}, {
        "x": 534529.128,
        "y": 6986223.553,
        "z": 126.00299999999697
      }, {"x": 534503.605, "y": 6986289.122, "z": 127.4149999999936}, {
        "x": 534484.224,
        "y": 6986347.087,
        "z": 128.66700000000128
      }, {"x": 534467.851, "y": 6986404.319, "z": 129.89999999999418}, {
        "x": 534460.339,
        "y": 6986438.878,
        "z": 130.56900000000314
      }, {"x": 534453.219, "y": 6986476.085, "z": 131.33900000000722}, {
        "x": 534443.512,
        "y": 6986534.336,
        "z": 132.62200000000303
      }, {"x": 534436.029, "y": 6986582.044, "z": 133.63499999999476}, {
        "x": 534429.677,
        "y": 6986622.664,
        "z": 134.4930000000022
      }, {"x": 534422.18, "y": 6986665.302, "z": 135.30599999999686}, {
        "x": 534411.664,
        "y": 6986712.914,
        "z": 136.14500000000407
      }, {"x": 534404.492, "y": 6986745.452, "z": 136.6030000000028}, {
        "x": 534393.21,
        "y": 6986783.352,
        "z": 137.09900000000198
      }, {"x": 534379.473, "y": 6986824.963, "z": 137.5170000000071}, {
        "x": 534371.852,
        "y": 6986847.324,
        "z": 137.70799999999872
      }, {"x": 534361.831, "y": 6986873.463, "z": 137.903999999995}, {
        "x": 534354.75,
        "y": 6986890.903,
        "z": 138.0350000000035
      }, {"x": 534322.352, "y": 6986961.042, "z": 138.20500000000175}, {
        "x": 534288.184,
        "y": 6987023.391,
        "z": 138.05499999999302
      }, {"x": 534257.936, "y": 6987071.896, "z": 137.77999999999884}, {
        "x": 534235.227,
        "y": 6987105.015,
        "z": 137.47999999999593
      }, {"x": 534183.023, "y": 6987172.302, "z": 136.4820000000036}, {
        "x": 534117.818,
        "y": 6987242.69,
        "z": 134.8509999999951
      }, {"x": 534036.836, "y": 6987315.69, "z": 132.39100000000326}, {
        "x": 534022.6530239584,
        "y": 6987326.98498092,
        "z": 131.922000792255
      }],
      "id": 245259,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5311,
      "endMValue": 2206.403,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827771,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174621,
      "startAddressM": 1888,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 258757,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535877.883, "y": 6984585.782, "z": 95.20100000000093}, {
        "x": 535873.8070059894,
        "y": 6984606.505969548,
        "z": 94.7480006656576
      }],
      "id": 258757,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1909,
      "endMValue": 21.121,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058942,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174720,
      "startAddressM": 1388,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 263913,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535868.978, "y": 6984089.904, "z": 100.88099999999395}, {
        "x": 535869.727,
        "y": 6984092.788,
        "z": 100.91199999999662
      }, {"x": 535879.685, "y": 6984141.191, "z": 100.95299999999406}, {
        "x": 535888.737,
        "y": 6984196.65,
        "z": 100.82099999999627
      }, {"x": 535895.118, "y": 6984249.771, "z": 100.54200000000128}, {
        "x": 535897.908,
        "y": 6984289.324,
        "z": 100.25500000000466
      }, {"x": 535899.593, "y": 6984345.469, "z": 99.63099999999395}, {
        "x": 535898.686,
        "y": 6984409.934,
        "z": 98.6929999999993
      }, {"x": 535896.2680274083, "y": 6984448.1845664205, "z": 98.04800731113939}],
      "id": 263913,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1749,
      "endMValue": 360.712,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1026476960,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172708,
      "startAddressM": 5311,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 197659,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 534022.653, "y": 6987326.985, "z": 131.92200000000594}, {
        "x": 533941.907,
        "y": 6987387.874,
        "z": 128.97299999999814
      }, {"x": 533892.712, "y": 6987423.515, "z": 126.93300000000454}, {
        "x": 533844.652,
        "y": 6987459.974,
        "z": 124.83199999999488
      }, {"x": 533797.917, "y": 6987498.421, "z": 122.71899999999732}, {
        "x": 533736.969,
        "y": 6987556.308,
        "z": 119.76300000000629
      }, {"x": 533679.816, "y": 6987618.757, "z": 116.81200000000536}, {
        "x": 533634.481,
        "y": 6987676.432,
        "z": 114.30899999999383
      }],
      "id": 197659,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5836,
      "endMValue": 524.792,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823096,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172711,
      "startAddressM": 5836,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 239774,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533634.481, "y": 6987676.432, "z": 114.30899999999383}, {
        "x": 533606.916,
        "y": 6987715.108,
        "z": 112.9890000000014
      }, {"x": 533593.916, "y": 6987734.971, "z": 112.37699999999313}],
      "id": 239774,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5907,
      "endMValue": 71.233,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 533593.9201995566, "y": 6987734.964583401, "z": 112.37719770220025},
        "value": 5907
      }],
      "mmlId": 318833978,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174608,
      "startAddressM": 2540,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 239847,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535604.072, "y": 6985172.361, "z": 89.8859999999986}, {
        "x": 535586.169,
        "y": 6985196.683,
        "z": 90.33299999999872
      }, {"x": 535535.699, "y": 6985260.234, "z": 91.71700000000419}, {
        "x": 535497.0261849023,
        "y": 6985304.455788567,
        "z": 93.0599935788815
      }],
      "id": 239847,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2710,
      "endMValue": 170.101,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1645159314,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172661,
      "startAddressM": 3087,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 245348,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535219.059, "y": 6985556.999, "z": 104.96799999999348}, {
        "x": 535204.9482716252,
        "y": 6985567.332801078,
        "z": 105.59298796925131
      }],
      "id": 245348,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3104,
      "endMValue": 17.49,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024013885,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174618,
      "startAddressM": 1749,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 262642,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535896.268, "y": 6984448.185, "z": 98.04799999999523}, {
        "x": 535892.73,
        "y": 6984486.531,
        "z": 97.29499999999825
      }, {"x": 535888.03, "y": 6984527.198, "z": 96.52999999999884}, {
        "x": 535878.604,
        "y": 6984582.041,
        "z": 95.27099999999336
      }, {"x": 535877.883, "y": 6984585.782, "z": 95.20100000000093}],
      "id": 262642,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1888,
      "endMValue": 138.904,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058954,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174604,
      "startAddressM": 2710,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 234960,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 2,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535497.026, "y": 6985304.456, "z": 93.05999999999767}, {
        "x": 535484.472,
        "y": 6985318.792,
        "z": 93.56900000000314
      }, {"x": 535442.201, "y": 6985363.669, "z": 95.17399999999907}, {
        "x": 535382.006,
        "y": 6985422.032,
        "z": 97.87799999999697
      }, {"x": 535320.322, "y": 6985477.154, "z": 100.66999999999825}, {
        "x": 535265.725,
        "y": 6985522.111,
        "z": 103.04099999999744
      }, {"x": 535226.582, "y": 6985551.48, "z": 104.67500000000291}, {
        "x": 535219.059,
        "y": 6985556.999,
        "z": 104.96799999999348
      }],
      "id": 234960,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3087,
      "endMValue": 376.265,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024013897,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171958,
      "startAddressM": 1554,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 218081,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533797.62, "y": 6990759.322, "z": 119.61199999999371}, {
        "x": 533800.425,
        "y": 6990763.814,
        "z": 119.75900000000547
      }, {"x": 533807.735, "y": 6990779.756, "z": 120.30400000000373}, {
        "x": 533815.276,
        "y": 6990798.36,
        "z": 120.86000000000058
      }, {"x": 533824.755, "y": 6990821.999, "z": 121.41599999999744}, {
        "x": 533833.885,
        "y": 6990842.486,
        "z": 121.74300000000221
      }, {"x": 533845.844, "y": 6990863.655, "z": 121.94500000000698}, {
        "x": 533857.375,
        "y": 6990878.74,
        "z": 121.96300000000338
      }, {"x": 533874.031, "y": 6990893.077, "z": 121.7100000000064}, {
        "x": 533886.873,
        "y": 6990901.874,
        "z": 121.50599999999395
      }],
      "id": 218081,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1725,
      "endMValue": 171.65,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833522,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171967,
      "startAddressM": 1443,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 187840,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533886.873, "y": 6990901.874, "z": 121.50599999999395}, {
        "x": 533892.316,
        "y": 6990905.254,
        "z": 121.45600000000559
      }, {"x": 533918.925, "y": 6990919.963, "z": 120.84100000000035}, {
        "x": 533952.438,
        "y": 6990936.234,
        "z": 120.17500000000291
      }, {"x": 533984.549, "y": 6990952.651, "z": 119.61500000000524}, {
        "x": 533985.3637187029,
        "y": 6990953.01587402,
        "z": 119.60500345149329
      }],
      "id": 187840,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1554,
      "endMValue": 111.022,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833534,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172089,
      "startAddressM": 4098,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 210251,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532904.851, "y": 6988393.642, "z": 97.16000000000349}, {
        "x": 532891.437,
        "y": 6988414.592,
        "z": 97.75
      }, {"x": 532879.195, "y": 6988439.615, "z": 98.57399999999325}, {
        "x": 532869.63,
        "y": 6988467.918,
        "z": 99.33199999999488
      }, {"x": 532864.637, "y": 6988507.179, "z": 100.26399999999558}, {
        "x": 532861.999,
        "y": 6988545.748,
        "z": 101.46400000000722
      }, {"x": 532860.393, "y": 6988579.945, "z": 102.23099999999977}, {
        "x": 532858.746,
        "y": 6988605.612,
        "z": 102.71400000000722
      }, {"x": 532857.986, "y": 6988634.643, "z": 103.30000000000291}, {
        "x": 532859.665,
        "y": 6988656.572,
        "z": 103.82499999999709
      }, {"x": 532862.896, "y": 6988675.556, "z": 104.58999999999651}, {
        "x": 532866.752,
        "y": 6988691.005,
        "z": 105.10000000000582
      }, {"x": 532866.9689830488, "y": 6988691.492961879, "z": 105.10999921883734}],
      "id": 210251,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4405,
      "endMValue": 307.548,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1267802104,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172097,
      "startAddressM": 3699,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 244445,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532963.239, "y": 6988851.919, "z": 110.41599999999744}, {
        "x": 532969.157,
        "y": 6988861.905,
        "z": 110.56600000000617
      }, {"x": 532979.486, "y": 6988881.589, "z": 110.8920000000071}, {
        "x": 532987.082,
        "y": 6988902.184,
        "z": 111.2039999999979
      }, {"x": 532993.359, "y": 6988929.615, "z": 111.56900000000314}, {
        "x": 532995.913,
        "y": 6988953.323,
        "z": 111.86699999999837
      }, {"x": 532995.474, "y": 6988976.666, "z": 112.03100000000268}, {
        "x": 532993.907,
        "y": 6989000.436,
        "z": 112.1530000000057
      }, {"x": 532994.107, "y": 6989023.648, "z": 111.98699999999371}, {
        "x": 532997.672,
        "y": 6989044.073,
        "z": 111.61900000000605
      }, {"x": 533002.493, "y": 6989057.755, "z": 111.33999999999651}],
      "id": 244445,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3912,
      "endMValue": 213.396,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 918600381,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172045,
      "startAddressM": 3148,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 201682,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533043.655, "y": 6989195.389, "z": 106.48699999999371}, {
        "x": 533043.665,
        "y": 6989195.577,
        "z": 106.48099999999977
      }, {"x": 533044.897, "y": 6989218.326, "z": 106.06100000000151}, {
        "x": 533045.891,
        "y": 6989247.035,
        "z": 106.49300000000221
      }, {"x": 533046.396, "y": 6989283.564, "z": 107.79399999999441}, {
        "x": 533045.795,
        "y": 6989321.868,
        "z": 109.07300000000396
      }, {"x": 533044.064, "y": 6989362.499, "z": 109.51399999999558}, {
        "x": 533040.681,
        "y": 6989389.914,
        "z": 109.68600000000151
      }, {"x": 533036.699, "y": 6989422.055, "z": 109.5850000000064}, {
        "x": 533032.348,
        "y": 6989451.725,
        "z": 108.97500000000582
      }, {"x": 533030.088, "y": 6989476.891, "z": 108.41199999999662}, {
        "x": 533030.33,
        "y": 6989500.976,
        "z": 108.07000000000698
      }, {"x": 533034.407, "y": 6989526.095, "z": 108.36999999999534}, {
        "x": 533041.282,
        "y": 6989545.077,
        "z": 108.62200000000303
      }, {"x": 533054.453, "y": 6989570.504, "z": 109.10700000000361}, {
        "x": 533065.861,
        "y": 6989587.144,
        "z": 109.65200000000186
      }, {"x": 533070.9569918775, "y": 6989592.213991919, "z": 109.7679998151027}],
      "id": 201682,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3555,
      "endMValue": 408.192,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1490260451,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172099,
      "startAddressM": 3555,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 190723,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533002.493, "y": 6989057.755, "z": 111.33999999999651}, {
        "x": 533003.047,
        "y": 6989059.19,
        "z": 111.30999999999767
      }, {"x": 533009.396, "y": 6989075.615, "z": 110.94800000000396}, {
        "x": 533017.076,
        "y": 6989094.236,
        "z": 110.43499999999767
      }, {"x": 533028.861, "y": 6989120.633, "z": 109.42999999999302}, {
        "x": 533036.808,
        "y": 6989144.175,
        "z": 108.49800000000687
      }, {"x": 533041.456, "y": 6989165.199, "z": 107.57000000000698}, {
        "x": 533043.6549813771,
        "y": 6989195.388744326,
        "z": 106.4870091717406
      }],
      "id": 190723,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3699,
      "endMValue": 144.847,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833294,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172013,
      "startAddressM": 2220,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 207021,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533460.326, "y": 6990355.025, "z": 110.52099999999336}, {
        "x": 533478.555,
        "y": 6990367.071,
        "z": 110.85099999999511
      }, {"x": 533494.878, "y": 6990375.869, "z": 111.16800000000512}],
      "id": 207021,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2260,
      "endMValue": 40.393,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1026507766,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171974,
      "startAddressM": 1749,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 248017,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533762.654, "y": 6990707.161, "z": 117.59200000000419}, {
        "x": 533764.175,
        "y": 6990708.724,
        "z": 117.66199999999662
      }, {"x": 533773.547, "y": 6990719.041, "z": 118.08999999999651}, {
        "x": 533786.629,
        "y": 6990736.914,
        "z": 118.81699999999546
      }, {"x": 533787.2747769256, "y": 6990738.110586655, "z": 118.86298411543663}],
      "id": 248017,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1788,
      "endMValue": 39.628,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833498,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172006,
      "startAddressM": 1841,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 247109,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533716.2446182341, "y": 6990666.588143618, "z": 117.70749647731529}, {
        "x": 533723.678,
        "y": 6990671.755,
        "z": 117.403999999995
      }],
      "id": 247109,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 74.429,
      "endAddressM": 1850,
      "endMValue": 83.482,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1483685539,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172003,
      "startAddressM": 1924,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 265377,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533494.878, "y": 6990375.869, "z": 111.16800000000512}, {
        "x": 533500.945,
        "y": 6990379.431,
        "z": 110.93300000000454
      }, {"x": 533515.643, "y": 6990390.752, "z": 111.17500000000291}, {
        "x": 533531.131,
        "y": 6990406.221,
        "z": 111.55599999999686
      }, {"x": 533548.781, "y": 6990427.994, "z": 112.63700000000244}, {
        "x": 533563.352,
        "y": 6990451.58,
        "z": 113.778999999995
      }, {"x": 533569.245, "y": 6990466.484, "z": 114.36299999999756}, {
        "x": 533578.376,
        "y": 6990485.367,
        "z": 115.1929999999993
      }, {"x": 533592.57, "y": 6990514.306, "z": 116.62600000000384}, {
        "x": 533604.529,
        "y": 6990538.613,
        "z": 117.6469999999972
      }, {"x": 533617.575, "y": 6990561.994, "z": 118.57600000000093}, {
        "x": 533622.782,
        "y": 6990571.917,
        "z": 118.94400000000314
      }, {"x": 533629.458, "y": 6990583.299, "z": 119.22000000000116}, {
        "x": 533644.214,
        "y": 6990602.712,
        "z": 119.48799999999756
      }, {"x": 533650.228, "y": 6990610.16, "z": 119.52599999999802}, {
        "x": 533659.1268236998,
        "y": 6990619.315818608,
        "z": 119.47400103018607
      }],
      "id": 265377,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2220,
      "endMValue": 297.455,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1026507778,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172119,
      "startAddressM": 4405,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 245209,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532951.371, "y": 6988297.534, "z": 95.83000000000175}, {
        "x": 532946.847,
        "y": 6988316.676,
        "z": 95.89900000000489
      }, {"x": 532937.298, "y": 6988340.243, "z": 96.2100000000064}, {
        "x": 532924.292,
        "y": 6988364.928,
        "z": 96.56600000000617
      }, {"x": 532904.8512366835, "y": 6988393.6416504225, "z": 97.15999276837786}],
      "id": 245209,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4512,
      "endMValue": 107.675,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833912,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172006,
      "startAddressM": 1850,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 247110,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533659.127, "y": 6990619.316, "z": 119.47400000000198}, {
        "x": 533660.031,
        "y": 6990620.218,
        "z": 119.48099999999977
      }, {"x": 533674.789, "y": 6990634.888, "z": 119.24000000000524}, {
        "x": 533694.642,
        "y": 6990652.574,
        "z": 118.56200000000536
      }, {"x": 533710.648, "y": 6990662.698, "z": 117.93600000000151}, {
        "x": 533716.2446182341,
        "y": 6990666.588143618,
        "z": 117.70749647731529
      }],
      "id": 247110,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1924,
      "endMValue": 74.429,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1483685539,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172122,
      "startAddressM": 4512,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 266618,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532928.468, "y": 6988172.991, "z": 100.48699999999371}, {
        "x": 532928.588,
        "y": 6988173.389,
        "z": 100.471000000005
      }, {"x": 532938.439, "y": 6988205.811, "z": 99.028999999995}, {
        "x": 532946.01,
        "y": 6988234.734,
        "z": 97.6710000000021
      }, {"x": 532950.698, "y": 6988263.941, "z": 96.29700000000594}, {
        "x": 532952.17,
        "y": 6988281.153,
        "z": 95.89299999999639
      }, {"x": 532951.44, "y": 6988297.044, "z": 95.82700000000477}, {
        "x": 532951.371,
        "y": 6988297.534,
        "z": 95.83000000000175
      }],
      "id": 266618,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4639,
      "endMValue": 127.457,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833156,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171956,
      "startAddressM": 1124,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 208129,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533985.364, "y": 6990953.016, "z": 119.60499999999593}, {
        "x": 534007.874,
        "y": 6990962.821,
        "z": 119.2390000000014
      }, {"x": 534030.654, "y": 6990975.825, "z": 118.90600000000268}, {
        "x": 534035.553,
        "y": 6990978.553,
        "z": 118.84299999999348
      }, {"x": 534044.326, "y": 6990983.545, "z": 118.76799999999639}, {
        "x": 534059.753,
        "y": 6990994.131,
        "z": 118.44700000000012
      }, {"x": 534077.315, "y": 6991006.904, "z": 118.10599999999977}, {
        "x": 534092.575,
        "y": 6991018.111,
        "z": 117.84200000000419
      }, {"x": 534115.994, "y": 6991034.002, "z": 117.63400000000547}, {
        "x": 534137.122,
        "y": 6991046.516,
        "z": 117.98200000000361
      }, {"x": 534161.19, "y": 6991058.811, "z": 118.4429999999993}, {
        "x": 534189.028,
        "y": 6991069.391,
        "z": 118.00400000000081
      }, {"x": 534217.045, "y": 6991080.675, "z": 117.14400000000023}, {
        "x": 534243.985,
        "y": 6991091.076,
        "z": 116.27599999999802
      }, {"x": 534267.021, "y": 6991099.909, "z": 115.66300000000047}, {
        "x": 534267.9276400746,
        "y": 6991100.136909523,
        "z": 115.62801388906541
      }],
      "id": 208129,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1443,
      "endMValue": 320.196,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833792,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172109,
      "startAddressM": 3912,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 240571,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532866.969, "y": 6988691.493, "z": 105.11000000000058}, {
        "x": 532869.758,
        "y": 6988696.677,
        "z": 105.21899999999732
      }, {"x": 532874.569, "y": 6988708.285, "z": 105.55899999999383}, {
        "x": 532884.216,
        "y": 6988727.227,
        "z": 106.30299999999988
      }, {"x": 532896.442, "y": 6988746.991, "z": 107.12799999999697}, {
        "x": 532913.85,
        "y": 6988775.211,
        "z": 108.24700000000303
      }, {"x": 532929.592, "y": 6988799.797, "z": 109.13300000000163}, {
        "x": 532943.265,
        "y": 6988820.89,
        "z": 109.71600000000035
      }, {"x": 532957.63, "y": 6988842.487, "z": 110.27300000000105}, {
        "x": 532963.2389981671,
        "y": 6988851.918996918,
        "z": 110.41599995326943
      }],
      "id": 240571,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4098,
      "endMValue": 187.349,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833270,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172088,
      "startAddressM": 4639,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 227075,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 532825.306, "y": 6987826.814, "z": 111.75599999999395}, {
        "x": 532828.213,
        "y": 6987840.634,
        "z": 111.32700000000477
      }, {"x": 532831.296, "y": 6987856.274, "z": 110.61900000000605}, {
        "x": 532836.595,
        "y": 6987894.354,
        "z": 108.82499999999709
      }, {"x": 532840.489, "y": 6987932.617, "z": 107.4600000000064}, {
        "x": 532845.266,
        "y": 6987976.502,
        "z": 106.02000000000407
      }, {"x": 532850.453, "y": 6988015.51, "z": 104.74599999999919}, {
        "x": 532860.188,
        "y": 6988042.571,
        "z": 103.90799999999581
      }, {"x": 532873.123, "y": 6988064.856, "z": 103.06100000000151}, {
        "x": 532885.561,
        "y": 6988083.708,
        "z": 102.05000000000291
      }, {"x": 532899.534, "y": 6988103.08, "z": 101.21799999999348}, {
        "x": 532910.305,
        "y": 6988122.958,
        "z": 100.83999999999651
      }, {"x": 532920.728, "y": 6988148.181, "z": 100.77300000000105}, {
        "x": 532928.468,
        "y": 6988172.991,
        "z": 100.48699999999371
      }],
      "id": 227075,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5005,
      "endMValue": 367.353,
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 532825.306, "y": 6987826.814, "z": 111.75599999999395}, "value": 5005}],
      "mmlId": 318833150,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172027,
      "startAddressM": 2260,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 251734,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533396.748, "y": 6990287.743, "z": 110.83000000000175}, {
        "x": 533396.925,
        "y": 6990288.141,
        "z": 110.83000000000175
      }, {"x": 533399.135, "y": 6990293.33, "z": 110.84100000000035}, {
        "x": 533407.558,
        "y": 6990307.788,
        "z": 110.88899999999558
      }, {"x": 533417.632, "y": 6990319.602, "z": 110.88899999999558}, {
        "x": 533434.421,
        "y": 6990335.617,
        "z": 110.86699999999837
      }, {"x": 533453.572, "y": 6990350.274, "z": 110.846000000005}, {
        "x": 533460.3256950952,
        "y": 6990355.02478552,
        "z": 110.52101467189834
      }],
      "id": 251734,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2354,
      "endMValue": 93.91,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833438,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172004,
      "startAddressM": 1788,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 197621,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533723.678, "y": 6990671.755, "z": 117.403999999995}, {
        "x": 533725.252,
        "y": 6990672.969,
        "z": 117.34200000000419
      }, {"x": 533738.757, "y": 6990684.445, "z": 116.9890000000014}, {
        "x": 533750.48,
        "y": 6990695.216,
        "z": 117.12200000000303
      }, {"x": 533762.654, "y": 6990707.161, "z": 117.59200000000419}],
      "id": 197621,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1841,
      "endMValue": 52.686,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833486,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172046,
      "startAddressM": 3021,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 187766,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533070.957, "y": 6989592.214, "z": 109.76799999999639}, {
        "x": 533072.65,
        "y": 6989595.115,
        "z": 109.84799999999814
      }, {"x": 533082.648, "y": 6989603.934, "z": 109.82499999999709}, {
        "x": 533098.106,
        "y": 6989614.729,
        "z": 109.60499999999593
      }, {"x": 533115.054, "y": 6989624.004, "z": 109.40899999999965}, {
        "x": 533134.6,
        "y": 6989637.348,
        "z": 109.3179999999993
      }, {"x": 533150.134, "y": 6989651.946, "z": 109.38199999999779}, {
        "x": 533166.792,
        "y": 6989672.941,
        "z": 110.24700000000303
      }],
      "id": 187766,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3148,
      "endMValue": 126.649,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833342,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172043,
      "startAddressM": 2590,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 234970,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533166.792, "y": 6989672.941, "z": 110.24700000000303}, {
        "x": 533167.042,
        "y": 6989673.336,
        "z": 110.27499999999418
      }, {"x": 533176.416, "y": 6989691.236, "z": 111.08699999999953}, {
        "x": 533187.854,
        "y": 6989725.776,
        "z": 112.25800000000163
      }, {"x": 533193.668, "y": 6989769.658, "z": 113.04099999999744}, {
        "x": 533198.885,
        "y": 6989818.266,
        "z": 114.08800000000338
      }, {"x": 533200.871, "y": 6989829.495, "z": 114.34900000000198}, {
        "x": 533205.669,
        "y": 6989850.207,
        "z": 114.69400000000314
      }, {"x": 533214.366, "y": 6989874.161, "z": 115.10099999999511}, {
        "x": 533223.846,
        "y": 6989896.197,
        "z": 115.53900000000431
      }, {"x": 533233.417, "y": 6989914.611, "z": 115.87399999999616}, {
        "x": 533233.674,
        "y": 6989915.031,
        "z": 115.88000000000466
      }, {"x": 533248.616, "y": 6989936.622, "z": 116.25}, {
        "x": 533261.15,
        "y": 6989954.262,
        "z": 116.27000000000407
      }, {"x": 533268.697, "y": 6989963.312, "z": 116.15200000000186}, {
        "x": 533281.841,
        "y": 6989981.328,
        "z": 115.84100000000035
      }, {"x": 533296.917, "y": 6990001.381, "z": 115.34399999999732}, {
        "x": 533315.163,
        "y": 6990027.014,
        "z": 114.72699999999895
      }, {"x": 533328.612, "y": 6990046.019, "z": 114.31600000000617}, {
        "x": 533337.8249859926,
        "y": 6990061.03497717,
        "z": 113.93700057623246
      }],
      "id": 234970,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3021,
      "endMValue": 433.025,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318835040,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171973,
      "startAddressM": 1725,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 213134,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533787.275, "y": 6990738.111, "z": 118.86299999999756}, {
        "x": 533796.784,
        "y": 6990757.864,
        "z": 119.53100000000268
      }, {"x": 533797.619845183, "y": 6990759.321729996, "z": 119.6119849997851}],
      "id": 213134,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1749,
      "endMValue": 23.603,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 718006939,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172025,
      "startAddressM": 2354,
      "roadNameFi": "Räimäntie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 454437,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16333,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533337.825, "y": 6990061.035, "z": 113.93700000000536}, {
        "x": 533348.268,
        "y": 6990083.847,
        "z": 113.07300000000396
      }, {"x": 533357.503, "y": 6990105.104, "z": 112.56299999999464}, {
        "x": 533367.257,
        "y": 6990128.826,
        "z": 112.24000000000524
      }, {"x": 533374.152, "y": 6990152.56, "z": 111.87799999999697}, {
        "x": 533376.372,
        "y": 6990159.074,
        "z": 111.82499999999709
      }, {"x": 533379.254, "y": 6990177.4, "z": 111.7670000000071}, {
        "x": 533380.454,
        "y": 6990191.19,
        "z": 111.55100000000675
      }, {"x": 533382.088, "y": 6990210.925, "z": 111.28299999999581}, {
        "x": 533382.764,
        "y": 6990231.802,
        "z": 111.12399999999616
      }, {"x": 533384.754, "y": 6990251.344, "z": 110.93300000000454}, {
        "x": 533389.583,
        "y": 6990271.36,
        "z": 110.91000000000349
      }, {"x": 533396.748, "y": 6990287.743, "z": 110.83000000000175}],
      "id": 454437,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2590,
      "endMValue": 236.71,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 983578209,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172589,
      "startAddressM": 175,
      "roadNameFi": "Konttimiehentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "Municipality",
      "segmentId": 454436,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 536541.893, "y": 6985716.362, "z": 107.14900000000489}, {
        "x": 536499.82,
        "y": 6985717.433,
        "z": 108.44100000000617
      }, {"x": 536458.806, "y": 6985718.486, "z": 111.27400000000489}, {
        "x": 536420.228,
        "y": 6985718.565,
        "z": 111.66899999999441
      }, {"x": 536381.732, "y": 6985719.096, "z": 112.69800000000396}, {
        "x": 536363.874,
        "y": 6985720.375,
        "z": 113.29799999999523
      }, {"x": 536347.465, "y": 6985724.368, "z": 113.43600000000151}, {
        "x": 536330.232,
        "y": 6985729.176,
        "z": 113.23799999999756
      }, {"x": 536312.169, "y": 6985735.619, "z": 112.9030000000057}, {
        "x": 536294.515,
        "y": 6985742.062,
        "z": 112.57000000000698
      }, {"x": 536284.398, "y": 6985745.581, "z": 112.48399999999674}],
      "id": 454436,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 432,
      "endMValue": 261.557,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832074,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172574,
      "startAddressM": 432,
      "roadNameFi": "Konttimiehentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "Municipality",
      "segmentId": 221159,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 536284.398, "y": 6985745.581, "z": 112.48399999999674}, {
        "x": 536281.112,
        "y": 6985747.265,
        "z": 112.51499999999942
      }, {"x": 536277.175, "y": 6985748.776, "z": 112.41800000000512}, {
        "x": 536271.525,
        "y": 6985750.521,
        "z": 112.62799999999697
      }],
      "id": 221159,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 445,
      "endMValue": 13.823,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 536271.5282846098, "y": 6985750.519985549, "z": 112.62787791715687},
        "value": 445
      }],
      "mmlId": 318832068,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 2,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172547,
      "startAddressM": 0,
      "roadNameFi": "Konttimiehentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "Municipality",
      "segmentId": 245468,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 536689.972, "y": 6985774.464, "z": 100.29399999999441}, {
        "x": 536701.1936788834,
        "y": 6985781.617795289,
        "z": 100.33499882679477
      }],
      "id": 245468,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 13,
      "endMValue": 13.308,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 536701.1936256307, "y": 6985781.61776134, "z": 100.33499863223412},
        "value": 0
      }],
      "mmlId": 318829894,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172440,
      "startAddressM": 13,
      "roadNameFi": "Konttimiehentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "Municipality",
      "segmentId": 234027,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 536541.893, "y": 6985716.362, "z": 107.14900000000489}, {
        "x": 536569.2,
        "y": 6985716.106,
        "z": 106.5109999999986
      }, {"x": 536584.191, "y": 6985716.882, "z": 105.94500000000698}, {
        "x": 536603.365,
        "y": 6985721.04,
        "z": 104.86299999999756
      }, {"x": 536622.857, "y": 6985728.269, "z": 103.58999999999651}, {
        "x": 536641.101,
        "y": 6985737.761,
        "z": 102.49000000000524
      }, {"x": 536656.758, "y": 6985749.138, "z": 101.50299999999697}, {
        "x": 536675.763,
        "y": 6985765.031,
        "z": 100.5
      }, {"x": 536686.866, "y": 6985772.483, "z": 100.31200000000536}, {
        "x": 536689.9717718443,
        "y": 6985774.463854482,
        "z": 100.2940013222098
      }],
      "id": 234027,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 175,
      "endMValue": 164.478,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023913685,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170888,
      "startAddressM": 3452,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 209645,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529048.224, "y": 6990529.179, "z": 90.91999999999825}, {
        "x": 529072.781,
        "y": 6990551.676,
        "z": 90.92699999999604
      }, {"x": 529097.859, "y": 6990574.17, "z": 91.85400000000664}, {
        "x": 529117.338,
        "y": 6990592.796,
        "z": 93.05400000000373
      }, {"x": 529135.317, "y": 6990612.134, "z": 94.29799999999523}, {
        "x": 529148.092,
        "y": 6990629.103,
        "z": 95.08199999999488
      }, {"x": 529149.4728380453, "y": 6990631.155759238, "z": 95.12299519179588}],
      "id": 209645,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3597,
      "endMValue": 144.062,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318805164,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170848,
      "startAddressM": 3647,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 240052,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529172.015, "y": 6990675.944, "z": 95.49700000000303}, {
        "x": 529179.32,
        "y": 6990701.4,
        "z": 95.0679999999993
      }, {"x": 529184.446, "y": 6990731.235, "z": 94.08900000000722}, {
        "x": 529186.653,
        "y": 6990762.647,
        "z": 92.6030000000028
      }, {"x": 529185.942, "y": 6990801.874, "z": 90.73600000000442}, {
        "x": 529185.626,
        "y": 6990832.813,
        "z": 90.14599999999336
      }, {"x": 529185.626, "y": 6990856.018, "z": 90.00699999999779}, {
        "x": 529187.045,
        "y": 6990884.116,
        "z": 90.6710000000021
      }, {"x": 529188.464, "y": 6990915.134, "z": 91.88300000000163}, {
        "x": 529192.071,
        "y": 6990943.758,
        "z": 92.83900000000722
      }],
      "id": 240052,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3917,
      "endMValue": 269.659,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318805068,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170860,
      "startAddressM": 4037,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 225274,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529231.598, "y": 6991056.65, "z": 91.43700000000536}, {
        "x": 529237.881,
        "y": 6991073.21,
        "z": 91.53299999999581
      }, {"x": 529249.905, "y": 6991103.183, "z": 92.58800000000338}, {
        "x": 529253.718,
        "y": 6991113.424,
        "z": 93.11100000000442
      }],
      "id": 225274,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4098,
      "endMValue": 60.935,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318806943,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170896,
      "startAddressM": 3422,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 219184,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529025.341, "y": 6990509.567, "z": 91.20200000000477}, {
        "x": 529037.216,
        "y": 6990520.105,
        "z": 91.07200000000012
      }, {"x": 529047.861, "y": 6990528.866, "z": 90.92399999999907}, {
        "x": 529048.2236240702,
        "y": 6990529.178675851,
        "z": 90.92000414247579
      }],
      "id": 219184,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3452,
      "endMValue": 30.142,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1099745280,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170893,
      "startAddressM": 3597,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 240940,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529149.473, "y": 6990631.156, "z": 95.12300000000687}, {
        "x": 529161.262,
        "y": 6990651.203,
        "z": 95.471000000005
      }, {"x": 529171.75, "y": 6990675.196, "z": 95.50199999999313}, {
        "x": 529172.0149457442,
        "y": 6990675.943846855,
        "z": 95.49700102369681
      }],
      "id": 240940,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3647,
      "endMValue": 50.235,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318805158,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170889,
      "startAddressM": 3344,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 225736,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 528966.144, "y": 6990457.91, "z": 92.24300000000221}, {
        "x": 528990.531,
        "y": 6990479.141,
        "z": 91.87300000000687
      }, {"x": 529014.188, "y": 6990499.663, "z": 91.38000000000466}, {
        "x": 529025.3406690996,
        "y": 6990509.566706156,
        "z": 91.20200528112056
      }],
      "id": 225736,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3422,
      "endMValue": 78.567,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318805050,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5170864,
      "startAddressM": 3917,
      "roadNameFi": "Kumpusentie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 219876,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16275,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529192.071, "y": 6990943.758, "z": 92.83900000000722}, {
        "x": 529193.982,
        "y": 6990954.23,
        "z": 92.93899999999849
      }, {"x": 529195.832, "y": 6990959.301, "z": 92.93600000000151}, {
        "x": 529196.82,
        "y": 6990964.465,
        "z": 92.98600000000442
      }, {"x": 529198.671, "y": 6990969.608, "z": 93.0}, {
        "x": 529200.523,
        "y": 6990974.751,
        "z": 92.98600000000442
      }, {"x": 529202.303, "y": 6990979.896, "z": 93.00400000000081}, {
        "x": 529203.434,
        "y": 6990985.056,
        "z": 92.96600000000035
      }, {"x": 529205.285, "y": 6990990.199, "z": 92.9320000000007}, {
        "x": 529208.989,
        "y": 6991000.486,
        "z": 92.68899999999849
      }, {"x": 529214.158, "y": 6991014.843, "z": 92.25699999999779}, {
        "x": 529218.146,
        "y": 6991025.051,
        "z": 91.93099999999686
      }, {"x": 529220.07, "y": 6991030.192, "z": 91.80400000000373}, {
        "x": 529225.139,
        "y": 6991040.373,
        "z": 91.56500000000233
      }, {"x": 529226.99, "y": 6991045.516, "z": 91.51300000000629}, {
        "x": 529231.598,
        "y": 6991056.65,
        "z": 91.43700000000536
      }],
      "id": 219876,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4037,
      "endMValue": 119.956,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318805182,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172684,
      "startAddressM": 4074,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 190013,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 533441.882, "y": 6987686.382, "z": 114.42900000000373}, {
        "x": 533427.411,
        "y": 6987685.082,
        "z": 113.94700000000012
      }, {"x": 533406.802, "y": 6987683.869, "z": 113.30000000000291}, {
        "x": 533383.238,
        "y": 6987684.958,
        "z": 112.41199999999662
      }, {"x": 533347.983, "y": 6987687.252, "z": 110.83900000000722}, {
        "x": 533303.334,
        "y": 6987689.735,
        "z": 108.5109999999986
      }],
      "id": 190013,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4213,
      "endMValue": 138.811,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833108,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172580,
      "startAddressM": 340,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 266416,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 535920.414, "y": 6985737.609, "z": 110.88999999999942}, {
        "x": 535914.764,
        "y": 6985738.172,
        "z": 110.98600000000442
      }, {"x": 535903.6402977644, "y": 6985738.860981557, "z": 111.29999159493154}],
      "id": 266416,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 356,
      "endMValue": 16.823,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023893224,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172673,
      "startAddressM": 2464,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 263108,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534419.223, "y": 6986493.028, "z": 130.2170000000042}, {
        "x": 534416.349,
        "y": 6986510.936,
        "z": 130.63400000000547
      }, {"x": 534404.384, "y": 6986589.463, "z": 132.7569999999978}, {
        "x": 534394.997,
        "y": 6986644.864,
        "z": 134.67299999999523
      }, {"x": 534384.124, "y": 6986691.835, "z": 136.13000000000466}, {
        "x": 534365.128,
        "y": 6986744.04,
        "z": 138.1750000000029
      }, {"x": 534346.992, "y": 6986781.269, "z": 139.57200000000012}, {
        "x": 534329.913,
        "y": 6986809.047,
        "z": 140.66700000000128
      }, {"x": 534305.555, "y": 6986842.361, "z": 142.0789999999979}, {
        "x": 534275.333,
        "y": 6986876.967,
        "z": 143.5329999999958
      }, {"x": 534229.057, "y": 6986917.704, "z": 145.53200000000652}, {
        "x": 534192.523,
        "y": 6986944.177,
        "z": 147.06299999999464
      }, {"x": 534148.939, "y": 6986969.018, "z": 147.2320000000036}, {
        "x": 534127.043,
        "y": 6986981.051,
        "z": 147.03100000000268
      }],
      "id": 263108,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3064,
      "endMValue": 600.682,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 718006962,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172694,
      "startAddressM": 3875,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 214138,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 533536.995, "y": 6987520.633, "z": 112.31299999999464}, {
        "x": 533524.5,
        "y": 6987533.938,
        "z": 111.76900000000023
      }, {"x": 533519.1151650656, "y": 6987539.639825217, "z": 111.57900582403906}],
      "id": 214138,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3901,
      "endMValue": 26.095,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833216,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172703,
      "startAddressM": 3266,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 236071,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534013.314, "y": 6987141.193, "z": 142.5850000000064}, {
        "x": 534012.767,
        "y": 6987141.99,
        "z": 142.53800000000047
      }, {"x": 534004.095, "y": 6987152.08, "z": 142.08800000000338}, {
        "x": 533991.579,
        "y": 6987165.368,
        "z": 141.78699999999662
      }, {"x": 533976.356, "y": 6987178.404, "z": 141.5500000000029}, {
        "x": 533951.574,
        "y": 6987198.082,
        "z": 141.14800000000105
      }, {"x": 533928.888, "y": 6987215.66, "z": 140.51399999999558}, {
        "x": 533910.664,
        "y": 6987230.063,
        "z": 139.8579999999929
      }, {"x": 533888.839, "y": 6987247.44, "z": 138.7850000000035}, {
        "x": 533864.512,
        "y": 6987267.445,
        "z": 137.19500000000698
      }, {"x": 533835.863, "y": 6987288.845, "z": 135.55999999999767}, {
        "x": 533804.079,
        "y": 6987313.238,
        "z": 133.56600000000617
      }, {"x": 533778.32, "y": 6987334.2, "z": 131.8680000000022}, {
        "x": 533752.184,
        "y": 6987354.837,
        "z": 129.53599999999278
      }, {"x": 533727.138, "y": 6987373.263, "z": 126.92799999999988}, {
        "x": 533701.398,
        "y": 6987393.681,
        "z": 124.30800000000454
      }, {"x": 533677.753, "y": 6987412.003, "z": 122.05800000000454}, {
        "x": 533650.661,
        "y": 6987433.304,
        "z": 119.91400000000431
      }, {"x": 533624.565, "y": 6987452.856, "z": 117.90099999999802}, {
        "x": 533605.075,
        "y": 6987467.911,
        "z": 116.45699999999488
      }, {"x": 533577.518, "y": 6987489.197, "z": 114.49099999999453}, {
        "x": 533559.369,
        "y": 6987503.679,
        "z": 113.3469999999943
      }, {"x": 533536.995, "y": 6987520.633, "z": 112.31299999999464}],
      "id": 236071,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3875,
      "endMValue": 609.542,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833090,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172139,
      "startAddressM": 5021,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 219322,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532601.274, "y": 6988010.983, "z": 109.5679999999993}, {
        "x": 532598.5,
        "y": 6988012.712,
        "z": 109.48799999999756
      }, {"x": 532575.039, "y": 6988025.565, "z": 108.86100000000442}, {
        "x": 532536.677,
        "y": 6988041.246,
        "z": 108.14999999999418
      }, {"x": 532472.111, "y": 6988061.544, "z": 106.73399999999674}, {
        "x": 532396.956,
        "y": 6988083.191,
        "z": 106.07499999999709
      }, {"x": 532370.2712764523, "y": 6988091.187917152, "z": 105.5880050452475}],
      "id": 219322,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5266,
      "endMValue": 245.212,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1016882271,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172680,
      "startAddressM": 4213,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 226068,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 533303.334, "y": 6987689.735, "z": 108.5109999999986}, {
        "x": 533277.809,
        "y": 6987691.154,
        "z": 107.17299999999523
      }, {"x": 533192.693, "y": 6987696.068, "z": 105.66599999999744}, {
        "x": 533142.733,
        "y": 6987699.228,
        "z": 106.8469999999943
      }, {"x": 533103.042, "y": 6987701.828, "z": 108.2719999999972}, {
        "x": 533066.015,
        "y": 6987703.75,
        "z": 109.52000000000407
      }, {"x": 533065.314, "y": 6987703.8, "z": 109.54300000000512}, {
        "x": 533049.733,
        "y": 6987705.255,
        "z": 110.12200000000303
      }, {"x": 533019.393, "y": 6987706.951, "z": 110.90099999999802}, {
        "x": 532986.27,
        "y": 6987710.41,
        "z": 111.41300000000047
      }, {"x": 532955.75, "y": 6987719.164, "z": 111.66999999999825}, {
        "x": 532928.042,
        "y": 6987733.922,
        "z": 111.87600000000384
      }, {"x": 532922.173, "y": 6987738.37, "z": 111.91000000000349}],
      "id": 226068,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4600,
      "endMValue": 388.285,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834014,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172741,
      "startAddressM": 2098,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 220538,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534502.313, "y": 6986136.04, "z": 117.98799999999756}, {
        "x": 534495.64,
        "y": 6986155.839,
        "z": 118.278999999995
      }, {"x": 534484.676, "y": 6986190.036, "z": 119.21499999999651}, {
        "x": 534473.058,
        "y": 6986232.117,
        "z": 120.94199999999546
      }, {"x": 534470.209, "y": 6986242.071, "z": 121.38000000000466}],
      "id": 220538,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2208,
      "endMValue": 110.814,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823882,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172738,
      "startAddressM": 2208,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 193639,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534470.209, "y": 6986242.071, "z": 121.38000000000466}, {
        "x": 534455.483,
        "y": 6986293.978,
        "z": 123.35400000000664
      }, {"x": 534445.408, "y": 6986335.631, "z": 125.22699999999895}, {
        "x": 534434.791,
        "y": 6986391.042,
        "z": 127.19000000000233
      }, {"x": 534419.223, "y": 6986493.028, "z": 130.2170000000042}],
      "id": 193639,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2464,
      "endMValue": 256.396,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823876,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172652,
      "startAddressM": 375,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 221957,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 535683.447, "y": 6985703.83, "z": 112.97400000000198}, {
        "x": 535710.087,
        "y": 6985715.392,
        "z": 113.01300000000629
      }, {"x": 535740.477, "y": 6985727.192, "z": 113.2719999999972}, {
        "x": 535741.206,
        "y": 6985727.441,
        "z": 113.28200000000652
      }, {"x": 535758.701, "y": 6985732.417, "z": 113.24499999999534}, {
        "x": 535785.0,
        "y": 6985737.687,
        "z": 113.13499999999476
      }, {"x": 535810.442, "y": 6985740.373, "z": 112.94899999999325}, {
        "x": 535845.274,
        "y": 6985741.531,
        "z": 112.64299999999639
      }, {"x": 535882.302, "y": 6985740.167, "z": 111.85099999999511}, {
        "x": 535884.9419043341,
        "y": 6985740.00500587,
        "z": 111.77700268154771
      }],
      "id": 221957,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 582,
      "endMValue": 207.555,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1164629733,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172730,
      "startAddressM": 3064,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 224191,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534127.043, "y": 6986981.051, "z": 147.03100000000268}, {
        "x": 534125.089,
        "y": 6986982.126,
        "z": 146.97299999999814
      }, {"x": 534107.679, "y": 6986993.096, "z": 146.47500000000582}, {
        "x": 534091.079,
        "y": 6987005.539,
        "z": 146.15899999999965
      }, {"x": 534066.417, "y": 6987030.5, "z": 146.04200000000128}, {
        "x": 534055.714,
        "y": 6987047.349,
        "z": 145.69899999999325
      }, {"x": 534046.43, "y": 6987065.723, "z": 144.8880000000063}, {
        "x": 534043.614,
        "y": 6987071.895,
        "z": 144.64999999999418
      }, {"x": 534036.757, "y": 6987088.433, "z": 144.1820000000007}, {
        "x": 534030.648,
        "y": 6987104.904,
        "z": 143.7719999999972
      }, {"x": 534019.838, "y": 6987128.969, "z": 143.03800000000047}, {
        "x": 534013.314,
        "y": 6987141.193,
        "z": 142.5850000000064
      }],
      "id": 224191,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3266,
      "endMValue": 201.683,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823870,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172659,
      "startAddressM": 1120,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 191385,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 535172.466, "y": 6985563.739, "z": 111.16999999999825}, {
        "x": 535126.533,
        "y": 6985570.986,
        "z": 110.53599999999278
      }, {"x": 535083.794, "y": 6985582.18, "z": 110.11199999999371}, {
        "x": 535050.419,
        "y": 6985595.613,
        "z": 110.0170000000071
      }, {"x": 535024.704, "y": 6985608.095, "z": 110.05199999999604}, {
        "x": 534997.5004433937,
        "y": 6985620.93379074,
        "z": 110.35199511034574
      }],
      "id": 191385,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1305,
      "endMValue": 185.324,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058822,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172620,
      "startAddressM": 0,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 219542,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 536250.584, "y": 6985664.848, "z": 110.9429999999993}, {
        "x": 536223.447,
        "y": 6985675.444,
        "z": 110.25100000000384
      }, {"x": 536195.759, "y": 6985688.888, "z": 109.91599999999744}],
      "id": 219542,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 60,
      "endMValue": 59.912,
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 536250.584, "y": 6985664.848, "z": 110.9429999999993}, "value": 0}],
      "mmlId": 318828365,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172677,
      "startAddressM": 4636,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 258248,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532895.868, "y": 6987762.076, "z": 112.00500000000466}, {
        "x": 532879.175,
        "y": 6987777.65,
        "z": 111.97500000000582
      }, {"x": 532853.397, "y": 6987801.251, "z": 111.95100000000093}, {
        "x": 532825.306199187,
        "y": 6987826.813818739,
        "z": 111.75600138269537
      }],
      "id": 258248,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4731,
      "endMValue": 95.761,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833174,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172878,
      "startAddressM": 5266,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 211639,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532370.271, "y": 6988091.188, "z": 105.58800000000338}, {
        "x": 532336.73,
        "y": 6988101.233,
        "z": 104.48799999999756
      }, {"x": 532283.4, "y": 6988117.105, "z": 102.42399999999907}, {
        "x": 532234.203,
        "y": 6988132.264,
        "z": 100.00699999999779
      }, {"x": 532219.194, "y": 6988136.057, "z": 99.47500000000582}, {
        "x": 532201.792,
        "y": 6988140.731,
        "z": 98.75599999999395
      }, {"x": 532181.855, "y": 6988144.412, "z": 97.65899999999965}, {
        "x": 532154.464,
        "y": 6988148.261,
        "z": 96.39900000000489
      }, {"x": 532137.283, "y": 6988150.993, "z": 95.76799999999639}],
      "id": 211639,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5507,
      "endMValue": 240.965,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832989,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172870,
      "startAddressM": 5554,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 201654,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531773.601, "y": 6988125.214, "z": 89.78399999999965}, {
        "x": 531792.907,
        "y": 6988126.142,
        "z": 89.86000000000058
      }, {"x": 531830.626, "y": 6988124.155, "z": 88.55299999999988}, {
        "x": 531855.72,
        "y": 6988119.248,
        "z": 88.11699999999837
      }, {"x": 531876.192, "y": 6988115.83, "z": 88.14400000000023}, {
        "x": 531894.308,
        "y": 6988115.581,
        "z": 88.32700000000477
      }, {"x": 531913.909, "y": 6988118.466, "z": 88.76200000000244}, {
        "x": 531933.334,
        "y": 6988124.79,
        "z": 89.23500000000058
      }, {"x": 531957.242, "y": 6988135.419, "z": 90.00299999999697}, {
        "x": 531977.028,
        "y": 6988143.878,
        "z": 90.8859999999986
      }, {"x": 532004.141, "y": 6988153.103, "z": 92.04499999999825}, {
        "x": 532022.767,
        "y": 6988156.643,
        "z": 92.58699999999953
      }, {"x": 532045.044, "y": 6988157.784, "z": 93.0399999999936}, {
        "x": 532068.108,
        "y": 6988157.319,
        "z": 93.63300000000163
      }, {"x": 532090.197, "y": 6988155.665, "z": 94.23500000000058}],
      "id": 201654,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5878,
      "endMValue": 324.59,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832617,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172579,
      "startAddressM": 312,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 239664,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 535948.119, "y": 6985734.787, "z": 110.72599999999511}, {
        "x": 535920.414350186,
        "y": 6985737.6089643305,
        "z": 110.88999792707021
      }],
      "id": 239664,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 340,
      "endMValue": 27.848,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023876493,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172666,
      "startAddressM": 1458,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 212486,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534856.387, "y": 6985680.901, "z": 109.45699999999488}, {
        "x": 534829.134,
        "y": 6985689.226,
        "z": 108.82399999999325
      }, {"x": 534814.495, "y": 6985693.569, "z": 108.81699999999546}, {
        "x": 534781.869,
        "y": 6985703.245,
        "z": 108.70500000000175
      }, {"x": 534761.457, "y": 6985710.543, "z": 109.00400000000081}, {
        "x": 534738.278,
        "y": 6985718.507,
        "z": 109.5219999999972
      }, {"x": 534713.6611451235, "y": 6985726.034955621, "z": 110.32199528379282}],
      "id": 212486,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1608,
      "endMValue": 149.725,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 930228797,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172162,
      "startAddressM": 4731,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 227274,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532825.306, "y": 6987826.814, "z": 111.75599999999395}, {
        "x": 532823.531,
        "y": 6987828.43,
        "z": 111.74000000000524
      }, {"x": 532809.496, "y": 6987840.81, "z": 111.63099999999395}, {
        "x": 532787.942,
        "y": 6987859.284,
        "z": 111.2960000000021
      }, {"x": 532736.521, "y": 6987902.764, "z": 110.73699999999371}, {
        "x": 532697.052,
        "y": 6987934.4,
        "z": 110.39900000000489
      }, {"x": 532672.203, "y": 6987956.201, "z": 110.53100000000268}, {
        "x": 532660.065,
        "y": 6987966.644,
        "z": 110.69599999999627
      }, {"x": 532633.555, "y": 6987987.148, "z": 110.79899999999907}, {
        "x": 532619.214,
        "y": 6987998.526,
        "z": 110.42399999999907
      }, {"x": 532601.274, "y": 6988010.983, "z": 109.5679999999993}],
      "id": 227274,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5021,
      "endMValue": 290.156,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833900,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172736,
      "startAddressM": 1608,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 262172,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534713.661, "y": 6985726.035, "z": 110.32200000000012}, {
        "x": 534690.402,
        "y": 6985735.079,
        "z": 111.0170000000071
      }, {"x": 534667.549, "y": 6985742.88, "z": 111.60599999999977}, {
        "x": 534651.832,
        "y": 6985750.518,
        "z": 112.13800000000629
      }, {"x": 534636.803, "y": 6985761.594, "z": 113.09399999999732}, {
        "x": 534626.402,
        "y": 6985771.698,
        "z": 114.07600000000093
      }, {"x": 534619.587, "y": 6985782.84, "z": 114.7390000000014}, {
        "x": 534614.396,
        "y": 6985793.931,
        "z": 115.29099999999744
      }, {"x": 534609.824, "y": 6985810.869, "z": 116.38199999999779}, {
        "x": 534609.668,
        "y": 6985830.739,
        "z": 118.07099999999627
      }, {"x": 534610.042, "y": 6985852.689, "z": 119.54099999999744}, {
        "x": 534609.903,
        "y": 6985869.411,
        "z": 120.65600000000268
      }, {"x": 534609.29, "y": 6985883.988, "z": 121.25400000000081}, {
        "x": 534608.298,
        "y": 6985892.05,
        "z": 121.3070000000007
      }, {"x": 534600.606, "y": 6985911.916, "z": 121.06100000000151}, {
        "x": 534592.247,
        "y": 6985939.026,
        "z": 121.0170000000071
      }, {"x": 534582.98, "y": 6985967.757, "z": 120.75900000000547}, {
        "x": 534571.605,
        "y": 6985997.953,
        "z": 120.27000000000407
      }, {"x": 534560.366, "y": 6986022.569, "z": 119.471000000005}, {
        "x": 534546.521,
        "y": 6986048.881,
        "z": 118.70799999999872
      }, {"x": 534522.517, "y": 6986091.305, "z": 118.41000000000349}, {
        "x": 534505.759,
        "y": 6986126.76,
        "z": 117.96199999999953
      }, {"x": 534502.313114432, "y": 6986136.0396918375, "z": 117.98799913660994}],
      "id": 262172,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2098,
      "endMValue": 490.638,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823888,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172879,
      "startAddressM": 5507,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 192766,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532137.283, "y": 6988150.993, "z": 95.76799999999639}, {
        "x": 532125.497,
        "y": 6988152.264,
        "z": 95.37399999999616
      }, {"x": 532109.659, "y": 6988153.961, "z": 94.87900000000081}, {
        "x": 532090.451,
        "y": 6988155.642,
        "z": 94.24000000000524
      }, {"x": 532090.1974431637, "y": 6988155.664959871, "z": 94.2350087236941}],
      "id": 192766,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5554,
      "endMValue": 47.319,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832605,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172901,
      "startAddressM": 5878,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 194486,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531666.699, "y": 6988064.607, "z": 84.91999999999825}, {
        "x": 531678.09,
        "y": 6988076.405,
        "z": 85.11900000000605
      }, {"x": 531690.276, "y": 6988086.622, "z": 85.82000000000698}, {
        "x": 531701.444,
        "y": 6988096.836,
        "z": 87.04499999999825
      }, {"x": 531715.666, "y": 6988107.06, "z": 88.05899999999383}, {
        "x": 531727.262,
        "y": 6988113.375,
        "z": 88.153999999995
      }, {"x": 531745.692, "y": 6988120.747, "z": 88.71300000000338}, {
        "x": 531762.925,
        "y": 6988123.979,
        "z": 89.44100000000617
      }, {"x": 531771.96, "y": 6988125.069, "z": 89.75}, {
        "x": 531773.6009080329,
        "y": 6988125.213991873,
        "z": 89.7839980945255
      }],
      "id": 194486,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6004,
      "endMValue": 126.287,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832659,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172681,
      "startAddressM": 4600,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 211126,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 532922.173, "y": 6987738.37, "z": 111.91000000000349}, {
        "x": 532905.612,
        "y": 6987752.986,
        "z": 112.0
      }, {"x": 532895.868001851, "y": 6987762.075998274, "z": 112.00499999905486}],
      "id": 211126,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4636,
      "endMValue": 35.414,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1016860096,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172647,
      "startAddressM": 582,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 454438,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 535451.065, "y": 6985606.216, "z": 117.28100000000268}, {
        "x": 535493.426,
        "y": 6985626.02,
        "z": 117.52000000000407
      }, {"x": 535538.424, "y": 6985643.42, "z": 117.04899999999907}, {
        "x": 535581.412,
        "y": 6985661.788,
        "z": 115.25699999999779
      }, {"x": 535649.334, "y": 6985689.026, "z": 113.33999999999651}, {
        "x": 535683.446885575,
        "y": 6985703.829950343,
        "z": 112.97400122767462
      }],
      "id": 454438,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 834,
      "endMValue": 252.121,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828383,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172660,
      "startAddressM": 997,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 249241,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 535294.415, "y": 6985562.504, "z": 113.05400000000373}, {
        "x": 535274.374,
        "y": 6985560.348,
        "z": 112.74300000000221
      }, {"x": 535240.737, "y": 6985558.699, "z": 112.22500000000582}, {
        "x": 535217.426,
        "y": 6985559.752,
        "z": 111.86599999999453
      }, {"x": 535190.226, "y": 6985561.413, "z": 111.44599999999627}, {
        "x": 535172.4661392062,
        "y": 6985563.7389817685,
        "z": 111.17000216333683
      }],
      "id": 249241,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1120,
      "endMValue": 122.331,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827693,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172689,
      "startAddressM": 4066,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 241115,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 533441.882, "y": 6987686.382, "z": 114.42900000000373}, {
        "x": 533449.853,
        "y": 6987687.799,
        "z": 114.6079999999929
      }],
      "id": 241115,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4074,
      "endMValue": 8.096,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833078,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172657,
      "startAddressM": 834,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 240193,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 535324.459, "y": 6985566.377, "z": 113.44400000000314}, {
        "x": 535366.458,
        "y": 6985575.841,
        "z": 114.45699999999488
      }, {"x": 535409.838, "y": 6985589.456, "z": 116.16599999999744}, {
        "x": 535451.0649925695,
        "y": 6985606.21599698,
        "z": 117.28099979904387
      }],
      "id": 240193,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 967,
      "endMValue": 133.022,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828377,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172904,
      "startAddressM": 6004,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 233946,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531660.39, "y": 6988058.624, "z": 84.74199999999837}, {
        "x": 531666.699,
        "y": 6988064.607,
        "z": 84.91999999999825
      }],
      "id": 233946,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 6013,
      "endMValue": 8.695,
      "linkType": 99,
      "calibrationPoints": [{"point": {"x": 531660.39, "y": 6988058.624, "z": 84.74199999999837}, "value": 6013}],
      "mmlId": 345368531,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172650,
      "startAddressM": 356,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 234629,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 535903.64, "y": 6985738.861, "z": 111.30000000000291}, {
        "x": 535884.942,
        "y": 6985740.005,
        "z": 111.77700000000186
      }],
      "id": 234629,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 375,
      "endMValue": 18.733,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023876477,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172609,
      "startAddressM": 60,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "Municipality",
      "segmentId": 264664,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 536195.759, "y": 6985688.888, "z": 109.91599999999744}, {
        "x": 536183.187,
        "y": 6985693.599,
        "z": 109.76300000000629
      }, {"x": 536181.96, "y": 6985694.03, "z": 109.74899999999616}, {
        "x": 536170.059,
        "y": 6985697.642,
        "z": 109.72999999999593
      }, {"x": 536151.6, "y": 6985702.032, "z": 109.71600000000035}, {
        "x": 536133.56,
        "y": 6985704.787,
        "z": 109.78900000000431
      }, {"x": 536105.034, "y": 6985709.378, "z": 109.93399999999383}, {
        "x": 536081.495,
        "y": 6985712.243,
        "z": 110.0969999999943
      }, {"x": 536057.468, "y": 6985715.963, "z": 110.20100000000093}, {
        "x": 536025.391,
        "y": 6985722.124,
        "z": 110.3530000000028
      }, {"x": 535995.364, "y": 6985726.912, "z": 110.50599999999395}, {
        "x": 535955.455,
        "y": 6985733.763,
        "z": 110.73200000000361
      }, {"x": 535948.119, "y": 6985734.787, "z": 110.72599999999511}],
      "id": 264664,
      "administrativeClassId": "2",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 312,
      "endMValue": 252.275,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023876474,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172670,
      "startAddressM": 1305,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 228012,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 534997.5, "y": 6985620.934, "z": 110.35199999999895}, {
        "x": 534995.649,
        "y": 6985621.808,
        "z": 110.38199999999779
      }, {"x": 534963.1, "y": 6985637.442, "z": 111.30000000000291}, {
        "x": 534931.01,
        "y": 6985652.068,
        "z": 111.4780000000028
      }, {"x": 534915.858, "y": 6985657.994, "z": 111.19400000000314}, {
        "x": 534896.697,
        "y": 6985666.376,
        "z": 110.74300000000221
      }, {"x": 534862.068, "y": 6985679.068, "z": 109.63199999999779}, {
        "x": 534856.387,
        "y": 6985680.901,
        "z": 109.45699999999488
      }],
      "id": 228012,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1458,
      "endMValue": 153.457,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827795,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172658,
      "startAddressM": 967,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 221113,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 535294.415, "y": 6985562.504, "z": 113.05400000000373}, {
        "x": 535324.459,
        "y": 6985566.377,
        "z": 113.44400000000314
      }],
      "id": 221113,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 997,
      "endMValue": 30.293,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827819,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172691,
      "startAddressM": 3901,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 2,
      "administrativeClassMML": "State",
      "segmentId": 197485,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 533519.115, "y": 6987539.64, "z": 111.5789999999979}, {
        "x": 533514.805,
        "y": 6987544.379,
        "z": 111.47500000000582
      }, {"x": 533502.26, "y": 6987560.617, "z": 111.33100000000559}, {
        "x": 533488.286,
        "y": 6987582.006,
        "z": 111.63499999999476
      }, {"x": 533477.831, "y": 6987602.746, "z": 112.30100000000675}, {
        "x": 533465.402,
        "y": 6987635.059,
        "z": 113.20699999999488
      }, {"x": 533454.061, "y": 6987671.683, "z": 114.30299999999988}, {
        "x": 533449.853,
        "y": 6987687.799,
        "z": 114.6079999999929
      }],
      "id": 197485,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4066,
      "endMValue": 165.318,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833210,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172125,
      "startAddressM": 511,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454511,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533410.299, "y": 6988210.906, "z": 110.028999999995}, {
        "x": 533393.575,
        "y": 6988265.696,
        "z": 110.59500000000116
      }, {"x": 533374.946, "y": 6988332.025, "z": 111.2899999999936}, {
        "x": 533365.26,
        "y": 6988364.756,
        "z": 111.66700000000128
      }],
      "id": 454511,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 672,
      "endMValue": 160.31506845144585,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834476,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172103,
      "startAddressM": 1340,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454516,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533244.289, "y": 6989018.034, "z": 111.36000000000058}, {
        "x": 533242.732,
        "y": 6989039.703,
        "z": 111.04399999999441
      }],
      "id": 454516,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1362,
      "endMValue": 21.724866167361313,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833240,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172130,
      "startAddressM": 696,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454513,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533359.524, "y": 6988387.26, "z": 111.97900000000664}, {
        "x": 533356.223,
        "y": 6988403.087,
        "z": 112.05899999999383
      }, {"x": 533350.231, "y": 6988423.725, "z": 112.24599999999919}],
      "id": 454513,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 734,
      "endMValue": 37.657832621059754,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1645159372,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5171954,
      "startAddressM": 2774,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454521,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533495.968, "y": 6990406.965, "z": 116.73200000000361}, {
        "x": 533500.099,
        "y": 6990426.737,
        "z": 116.56100000000151
      }, {"x": 533510.869, "y": 6990485.645, "z": 116.13099999999395}, {
        "x": 533521.977,
        "y": 6990557.398,
        "z": 115.41800000000512
      }, {"x": 533528.866, "y": 6990617.32, "z": 114.77000000000407}, {
        "x": 533534.22,
        "y": 6990677.312,
        "z": 114.00299999999697
      }, {"x": 533539.491, "y": 6990748.596, "z": 113.1420000000071}, {
        "x": 533545.598,
        "y": 6990821.451,
        "z": 112.27999999999884
      }, {"x": 533553.145, "y": 6990893.255, "z": 111.403999999995}, {
        "x": 533560.307,
        "y": 6990942.381,
        "z": 110.80499999999302
      }, {"x": 533569.651, "y": 6990992.58, "z": 110.24400000000605}, {
        "x": 533582.273,
        "y": 6991049.06,
        "z": 109.58199999999488
      }, {"x": 533586.636, "y": 6991065.247, "z": 109.35700000000361}, {
        "x": 533588.248,
        "y": 6991070.653,
        "z": 109.3179999999993
      }],
      "id": 454521,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3448,
      "endMValue": 671.0125267405186,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833408,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172002,
      "startAddressM": 2702,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454520,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533480.398, "y": 6990336.817, "z": 117.05499999999302}, {
        "x": 533485.112,
        "y": 6990358.849,
        "z": 116.95299999999406
      }, {"x": 533491.395, "y": 6990385.347, "z": 116.83100000000559}, {
        "x": 533495.968,
        "y": 6990406.965,
        "z": 116.73200000000361
      }],
      "id": 454520,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2774,
      "endMValue": 71.8597521512611,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833402,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172036,
      "startAddressM": 1362,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454517,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533242.732, "y": 6989039.703, "z": 111.04399999999441}, {
        "x": 533239.089,
        "y": 6989084.046,
        "z": 110.5570000000007
      }, {"x": 533234.349, "y": 6989145.139, "z": 110.16199999999662}, {
        "x": 533230.287,
        "y": 6989216.397,
        "z": 110.05800000000454
      }, {"x": 533227.761, "y": 6989299.916, "z": 110.22000000000116}, {
        "x": 533228.404,
        "y": 6989372.247,
        "z": 110.6359999999986
      }, {"x": 533231.623, "y": 6989443.577, "z": 111.2960000000021}, {
        "x": 533239.125,
        "y": 6989525.939,
        "z": 112.04899999999907
      }, {"x": 533246.02, "y": 6989576.044, "z": 112.58699999999953}, {
        "x": 533247.677,
        "y": 6989591.522,
        "z": 112.83400000000256
      }],
      "id": 454517,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1917,
      "endMValue": 553.2829130850168,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833246,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172132,
      "startAddressM": 672,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454512,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533365.26, "y": 6988364.756, "z": 111.66700000000128}, {
        "x": 533359.524,
        "y": 6988387.26,
        "z": 111.97900000000664
      }],
      "id": 454512,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 696,
      "endMValue": 23.223516357088933,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834494,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172055,
      "startAddressM": 1917,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454518,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533247.677, "y": 6989591.522, "z": 112.83400000000256}, {
        "x": 533252.017,
        "y": 6989615.309,
        "z": 113.03200000000652
      }],
      "id": 454518,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1941,
      "endMValue": 24.179680912336334,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833252,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172091,
      "startAddressM": 734,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454514,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533350.231, "y": 6988423.725, "z": 112.24599999999919}, {
        "x": 533341.188,
        "y": 6988460.23,
        "z": 112.53699999999662
      }, {"x": 533327.561, "y": 6988514.938, "z": 113.028999999995}, {
        "x": 533316.53,
        "y": 6988561.507,
        "z": 113.4149999999936
      }, {"x": 533299.757, "y": 6988642.994, "z": 113.67600000000675}, {
        "x": 533284.518,
        "y": 6988725.655,
        "z": 113.58000000000175
      }, {"x": 533270.822, "y": 6988809.254, "z": 113.16300000000047}, {
        "x": 533259.436,
        "y": 6988892.555,
        "z": 112.38000000000466
      }, {"x": 533249.153, "y": 6988975.982, "z": 111.625}, {
        "x": 533245.308,
        "y": 6989012.096,
        "z": 111.29899999999907
      }, {"x": 533244.289, "y": 6989018.034, "z": 111.36000000000058}],
      "id": 454514,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1340,
      "endMValue": 604.2852198046263,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834500,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172715,
      "startAddressM": 0,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454509,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533608.729, "y": 6987743.106, "z": 112.32799999999406}, {
        "x": 533599.85,
        "y": 6987757.247,
        "z": 111.9210000000021
      }],
      "id": 454509,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 17,
      "endMValue": 16.69744058299953,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1026522916,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172040,
      "startAddressM": 1941,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454519,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533252.017, "y": 6989615.309, "z": 113.03200000000652}, {
        "x": 533260.157,
        "y": 6989658.769,
        "z": 113.3859999999986
      }, {"x": 533272.931, "y": 6989717.756, "z": 113.92699999999604}, {
        "x": 533291.188,
        "y": 6989787.43,
        "z": 114.64500000000407
      }, {"x": 533301.189, "y": 6989822.029, "z": 115.02099999999336}, {
        "x": 533312.333,
        "y": 6989856.829,
        "z": 115.39500000000407
      }, {"x": 533327.662, "y": 6989901.018, "z": 115.86199999999371}, {
        "x": 533344.651,
        "y": 6989947.094,
        "z": 116.24499999999534
      }, {"x": 533366.495, "y": 6990003.148, "z": 116.68700000000536}, {
        "x": 533388.522,
        "y": 6990058.233,
        "z": 116.98799999999756
      }, {"x": 533410.345, "y": 6990114.773, "z": 117.19700000000012}, {
        "x": 533434.797,
        "y": 6990182.726,
        "z": 117.27999999999884
      }, {"x": 533453.025, "y": 6990240.153, "z": 117.28200000000652}, {
        "x": 533472.619,
        "y": 6990309.214,
        "z": 117.14299999999639
      }, {"x": 533479.108, "y": 6990332.652, "z": 117.05800000000454}, {
        "x": 533480.398,
        "y": 6990336.817,
        "z": 117.05499999999302
      }],
      "id": 454519,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2702,
      "endMValue": 758.0597896493481,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318833936,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172031,
      "startAddressM": 17,
      "roadNameFi": "VT 5",
      "roadPartNumber": 205,
      "administrativeClassMML": "State",
      "segmentId": 454510,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533599.85, "y": 6987757.247, "z": 111.9210000000021}, {
        "x": 533594.193,
        "y": 6987766.278,
        "z": 111.70100000000093
      }, {"x": 533564.528, "y": 6987817.922, "z": 110.48099999999977}, {
        "x": 533518.163,
        "y": 6987914.769,
        "z": 109.00500000000466
      }, {"x": 533481.347, "y": 6988005.207, "z": 108.58199999999488}, {
        "x": 533456.218,
        "y": 6988073.087,
        "z": 108.74400000000605
      }, {"x": 533424.923, "y": 6988165.432, "z": 109.55199999999604}, {
        "x": 533410.299,
        "y": 6988210.906,
        "z": 110.028999999995
      }],
      "id": 454510,
      "administrativeClassId": "1",
      "status": 0,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 511,
      "endMValue": 492.8854830407842,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834470,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174612,
      "startAddressM": 1915,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 202744,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535884.329, "y": 6984614.402, "z": 94.67299999999523}, {
        "x": 535870.449,
        "y": 6984674.011,
        "z": 93.26200000000244
      }, {"x": 535853.545, "y": 6984732.019, "z": 92.00900000000547}, {
        "x": 535830.706,
        "y": 6984799.241,
        "z": 90.72900000000664
      }, {"x": 535807.275, "y": 6984854.935, "z": 89.80299999999988}, {
        "x": 535781.478,
        "y": 6984910.242,
        "z": 89.17900000000373
      }, {"x": 535758.653, "y": 6984954.718, "z": 88.84100000000035}, {
        "x": 535737.273,
        "y": 6984994.566,
        "z": 88.70200000000477
      }, {"x": 535725.831, "y": 6985015.701, "z": 88.66999999999825}],
      "id": 202744,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2348,
      "endMValue": 433.316,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827699,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172662,
      "startAddressM": 3075,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 225964,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535246.234, "y": 6985557.278, "z": 104.51399999999558}, {
        "x": 535233.569007629,
        "y": 6985566.513994437,
        "z": 104.83199980844219
      }],
      "id": 225964,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3091,
      "endMValue": 15.675,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058789,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172718,
      "startAddressM": 5317,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 246953,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 534033.176, "y": 6987339.485, "z": 131.9360000000015}, {
        "x": 534003.677,
        "y": 6987363.014,
        "z": 130.95100000000093
      }, {"x": 533934.838, "y": 6987413.705, "z": 128.2679999999964}, {
        "x": 533897.533,
        "y": 6987441.019,
        "z": 126.6710000000021
      }],
      "id": 246953,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5487,
      "endMValue": 169.458,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834440,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172035,
      "startAddressM": 3091,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 264564,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535233.569, "y": 6985566.514, "z": 104.83199999999488}, {
        "x": 535191.502,
        "y": 6985597.18,
        "z": 106.58999999999651
      }, {"x": 535140.458, "y": 6985631.628, "z": 108.61800000000221}, {
        "x": 535091.172,
        "y": 6985663.065,
        "z": 110.55299999999988
      }, {"x": 535025.385, "y": 6985703.647, "z": 113.15799999999581}, {
        "x": 534976.526,
        "y": 6985733.668,
        "z": 114.94700000000012
      }, {"x": 534932.6, "y": 6985762.194, "z": 116.38000000000466}, {
        "x": 534897.878,
        "y": 6985786.858,
        "z": 117.44500000000698
      }, {"x": 534861.284, "y": 6985814.751, "z": 118.39500000000407}, {
        "x": 534827.005,
        "y": 6985844.175,
        "z": 119.22999999999593
      }, {"x": 534814.494, "y": 6985855.202, "z": 119.46099999999569}, {
        "x": 534791.982,
        "y": 6985875.83,
        "z": 119.89800000000105
      }, {"x": 534754.307, "y": 6985913.875, "z": 120.57600000000093}, {
        "x": 534719.224,
        "y": 6985951.468,
        "z": 121.22900000000664
      }, {"x": 534687.07, "y": 6985991.742, "z": 121.85899999999674}, {
        "x": 534657.823,
        "y": 6986030.306,
        "z": 122.43300000000454
      }, {"x": 534645.484, "y": 6986047.83, "z": 122.69599999999627}, {
        "x": 534608.89,
        "y": 6986104.531,
        "z": 123.53800000000047
      }, {"x": 534586.203, "y": 6986144.373, "z": 124.2329999999929}, {
        "x": 534568.346,
        "y": 6986178.373,
        "z": 124.87200000000303
      }, {"x": 534548.211, "y": 6986220.806, "z": 125.74300000000221}, {
        "x": 534530.011,
        "y": 6986263.879,
        "z": 126.72699999999895
      }, {"x": 534513.011, "y": 6986311.807, "z": 127.79300000000512}, {
        "x": 534495.098,
        "y": 6986368.564,
        "z": 129.00299999999697
      }, {"x": 534485.739, "y": 6986403.616, "z": 129.73799999999756}, {
        "x": 534475.071,
        "y": 6986450.444,
        "z": 130.78200000000652
      }, {"x": 534462.728, "y": 6986518.922, "z": 132.25900000000547}, {
        "x": 534453.517,
        "y": 6986578.167,
        "z": 133.4759999999951
      }, {"x": 534444.788, "y": 6986629.448, "z": 134.50400000000081}, {
        "x": 534433.824,
        "y": 6986688.637,
        "z": 135.65099999999802
      }, {"x": 534426.951, "y": 6986722.815, "z": 136.2280000000028}, {
        "x": 534408.207,
        "y": 6986792.919,
        "z": 137.1359999999986
      }, {"x": 534391.536, "y": 6986843.096, "z": 137.66199999999662}, {
        "x": 534356.783,
        "y": 6986927.894,
        "z": 138.0439999999944
      }, {"x": 534324.777, "y": 6986992.729, "z": 138.13499999999476}, {
        "x": 534278.074,
        "y": 6987071.897,
        "z": 137.82799999999406
      }, {"x": 534269.087, "y": 6987085.511, "z": 137.74599999999919}, {
        "x": 534226.335,
        "y": 6987144.756,
        "z": 137.04200000000128
      }, {"x": 534147.378, "y": 6987236.851, "z": 135.31900000000314}, {
        "x": 534068.438,
        "y": 6987311.322,
        "z": 133.04099999999744
      }, {"x": 534033.1763753364, "y": 6987339.484700227, "z": 131.93601176186274}],
      "id": 264564,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5317,
      "endMValue": 2228.83,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1645159342,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174719,
      "startAddressM": 1500,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 257473,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535901.92, "y": 6984201.437, "z": 100.7960000000021}, {
        "x": 535907.626,
        "y": 6984251.833,
        "z": 100.5170000000071
      }, {"x": 535911.652, "y": 6984322.719, "z": 99.91400000000431}, {
        "x": 535912.165,
        "y": 6984389.257,
        "z": 99.0219999999972
      }, {"x": 535907.67, "y": 6984460.892, "z": 97.86199999999371}, {
        "x": 535899.17,
        "y": 6984534.007,
        "z": 96.42200000000594
      }, {"x": 535889.09, "y": 6984593.488, "z": 95.11100000000442}, {
        "x": 535888.4520680612,
        "y": 6984596.289701086,
        "z": 94.97501450833664
      }],
      "id": 257473,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1896,
      "endMValue": 396.844,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058978,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174606,
      "startAddressM": 2717,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 216738,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535510.072, "y": 6985314.617, "z": 93.00100000000384}, {
        "x": 535507.647,
        "y": 6985317.395,
        "z": 93.08299999999872
      }, {"x": 535466.212, "y": 6985362.327, "z": 94.67699999999604}, {
        "x": 535423.849,
        "y": 6985404.821,
        "z": 96.4719999999943
      }, {"x": 535380.859, "y": 6985445.585, "z": 98.47400000000198}, {
        "x": 535336.508,
        "y": 6985484.872,
        "z": 100.50199999999313
      }, {"x": 535287.932, "y": 6985525.308, "z": 102.57700000000477}, {
        "x": 535248.45,
        "y": 6985555.662,
        "z": 104.45200000000477
      }, {"x": 535246.2342781334, "y": 6985557.277797174, "z": 104.51399221828703}],
      "id": 216738,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3075,
      "endMValue": 359.052,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058801,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174622,
      "startAddressM": 1896,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 234302,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535888.452, "y": 6984596.29, "z": 94.97500000000582}, {
        "x": 535884.3290779573,
        "y": 6984614.401657539,
        "z": 94.67300571018596
      }],
      "id": 234302,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1915,
      "endMValue": 18.575,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1024058966,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174605,
      "startAddressM": 2348,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 220737,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 535725.831, "y": 6985015.701, "z": 88.66999999999825}, {
        "x": 535720.702,
        "y": 6985024.886,
        "z": 88.69800000000396
      }, {"x": 535693.941, "y": 6985067.788, "z": 88.77599999999802}, {
        "x": 535654.507,
        "y": 6985128.468,
        "z": 89.21600000000035
      }, {"x": 535620.275, "y": 6985177.515, "z": 89.81200000000536}, {
        "x": 535587.832,
        "y": 6985221.378,
        "z": 90.65600000000268
      }, {"x": 535546.552, "y": 6985272.293, "z": 91.78399999999965}, {
        "x": 535510.072,
        "y": 6985314.617,
        "z": 93.00100000000384
      }],
      "id": 220737,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2717,
      "endMValue": 369.244,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1645159320,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172706,
      "startAddressM": 5487,
      "roadNameFi": "VT 5",
      "roadPartNumber": 203,
      "administrativeClassMML": "State",
      "segmentId": 242200,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5,
      "trackCode": 1,
      "roadClass": 1,
      "sideCode": 2,
      "points": [{"x": 533897.533, "y": 6987441.019, "z": 126.6710000000021}, {
        "x": 533895.837,
        "y": 6987442.261,
        "z": 126.57200000000012
      }, {"x": 533847.84, "y": 6987479.111, "z": 124.48699999999371}, {
        "x": 533785.981,
        "y": 6987532.151,
        "z": 121.56399999999849
      }, {"x": 533732.361, "y": 6987584.945, "z": 118.94199999999546}, {
        "x": 533684.755,
        "y": 6987638.89,
        "z": 116.346000000005
      }, {"x": 533640.921, "y": 6987695.843, "z": 113.98600000000442}, {
        "x": 533608.7291154703,
        "y": 6987743.105830471,
        "z": 112.32800594711262
      }],
      "id": 242200,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5907,
      "endMValue": 420.347,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 533608.7426377088, "y": 6987743.085977664, "z": 112.32870238943893},
        "value": 5907
      }],
      "mmlId": 318834458,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172555,
      "startAddressM": 1816,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 251546,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536820.212, "y": 6985607.936, "z": 99.02700000000186}, {
        "x": 536809.246,
        "y": 6985623.414,
        "z": 99.21499999999651
      }, {"x": 536793.089, "y": 6985646.31, "z": 99.38700000000244}, {
        "x": 536780.138215267,
        "y": 6985664.498697668,
        "z": 99.42599935176248
      }],
      "id": 251546,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1886,
      "endMValue": 69.32,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827735,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172497,
      "startAddressM": 2470,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 208028,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536617.238, "y": 6986213.339, "z": 97.2280000000028}, {
        "x": 536616.72,
        "y": 6986217.692,
        "z": 97.18099999999686
      }, {"x": 536615.781, "y": 6986224.11, "z": 97.19400000000314}, {
        "x": 536615.198,
        "y": 6986228.716,
        "z": 97.2039999999979
      }],
      "id": 208028,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2486,
      "endMValue": 15.513,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828185,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172492,
      "startAddressM": 2486,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 233642,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536615.198, "y": 6986228.716, "z": 97.2039999999979}, {
        "x": 536615.123,
        "y": 6986229.165,
        "z": 97.20500000000175
      }, {"x": 536606.772, "y": 6986287.402, "z": 98.00100000000384}, {
        "x": 536606.660001365,
        "y": 6986288.173990591,
        "z": 98.0089999025067
      }],
      "id": 233642,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2546,
      "endMValue": 60.068,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828161,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174525,
      "startAddressM": 844,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 241900,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537027.521, "y": 6984671.256, "z": 106.53699999999662}, {
        "x": 537007.644,
        "y": 6984800.149,
        "z": 100.31100000000151
      }, {"x": 537007.4280722997, "y": 6984801.381587289, "z": 100.2630160666163}],
      "id": 241900,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 977,
      "endMValue": 131.668,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827945,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174523,
      "startAddressM": 732,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 194131,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537046.585, "y": 6984561.149, "z": 111.81600000000617}, {
        "x": 537027.652,
        "y": 6984670.408,
        "z": 106.56900000000314
      }, {"x": 537027.5210508411, "y": 6984671.255670891, "z": 106.53701241920157}],
      "id": 194131,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 844,
      "endMValue": 111.745,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827939,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174495,
      "startAddressM": 1119,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 204913,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536986.661, "y": 6984941.733, "z": 95.50199999999313}, {
        "x": 536986.548,
        "y": 6984943.126,
        "z": 95.46199999999953
      }, {"x": 536968.72, "y": 6985052.068, "z": 92.06500000000233}, {
        "x": 536959.49,
        "y": 6985106.285,
        "z": 91.05999999999767
      }, {"x": 536948.467, "y": 6985169.752, "z": 92.47000000000116}, {
        "x": 536934.424,
        "y": 6985260.459,
        "z": 95.21700000000419
      }, {"x": 536934.066, "y": 6985263.179, "z": 95.26200000000244}],
      "id": 204913,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1446,
      "endMValue": 325.734,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827921,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 6565590,
      "startAddressM": 2231,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 201811,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536653.751, "y": 6985977.945, "z": 100.80400000000373}, {
        "x": 536651.675,
        "y": 6985987.35,
        "z": 100.67799999999988
      }, {"x": 536649.792, "y": 6985999.694, "z": 100.50199999999313}, {
        "x": 536647.437,
        "y": 6986015.0,
        "z": 100.17500000000291
      }, {"x": 536645.374, "y": 6986026.259, "z": 100.00299999999697}, {
        "x": 536645.181,
        "y": 6986027.371,
        "z": 99.9780000000028
      }, {"x": 536633.894, "y": 6986099.352, "z": 98.70699999999488}, {
        "x": 536627.1640069999,
        "y": 6986147.598949819,
        "z": 97.88800085184897
      }],
      "id": 201811,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2404,
      "endMValue": 171.754,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 534655304,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172546,
      "startAddressM": 1986,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 213572,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536723.424, "y": 6985746.7, "z": 99.81399999999849}, {
        "x": 536714.243,
        "y": 6985762.235,
        "z": 100.00400000000081
      }, {"x": 536701.1941620187, "y": 6985781.617759337, "z": 100.33499589025237}],
      "id": 213572,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2028,
      "endMValue": 41.411,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827957,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172471,
      "startAddressM": 2654,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 227029,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536590.154, "y": 6986394.012, "z": 100.22900000000664}, {
        "x": 536587.164,
        "y": 6986411.786,
        "z": 100.5280000000057
      }, {"x": 536584.363, "y": 6986427.666, "z": 100.8179999999993}],
      "id": 227029,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2688,
      "endMValue": 34.149,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023991697,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174503,
      "startAddressM": 1446,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 257173,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536934.066, "y": 6985263.179, "z": 95.26200000000244}, {
        "x": 536908.905,
        "y": 6985419.563,
        "z": 96.71099999999569
      }, {"x": 536896.257, "y": 6985473.784, "z": 98.28500000000349}],
      "id": 257173,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1661,
      "endMValue": 214.072,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827717,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5172490,
      "startAddressM": 2546,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 221064,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536606.66, "y": 6986288.174, "z": 98.00900000000547}, {
        "x": 536604.113,
        "y": 6986308.572,
        "z": 98.50199999999313
      }, {"x": 536600.822, "y": 6986330.79, "z": 99.1079999999929}, {
        "x": 536598.1430080006,
        "y": 6986342.923963763,
        "z": 99.41599908019187
      }],
      "id": 221064,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2602,
      "endMValue": 55.443,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828167,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172542,
      "startAddressM": 2148,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 222348,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536666.272, "y": 6985896.185, "z": 101.27099999999336}, {
        "x": 536666.224,
        "y": 6985896.485,
        "z": 101.27000000000407
      }, {"x": 536659.339, "y": 6985940.29, "z": 101.16099999999278}, {
        "x": 536653.751,
        "y": 6985977.945,
        "z": 100.80400000000373
      }],
      "id": 222348,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2231,
      "endMValue": 82.714,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828119,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172491,
      "startAddressM": 2602,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 255465,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536598.143, "y": 6986342.924, "z": 99.41599999999744}, {
        "x": 536598.105,
        "y": 6986343.167,
        "z": 99.41999999999825
      }, {"x": 536591.862, "y": 6986383.86, "z": 100.07000000000698}, {
        "x": 536590.154,
        "y": 6986394.012,
        "z": 100.22900000000664
      }],
      "id": 255465,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2654,
      "endMValue": 51.71,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828131,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174535,
      "startAddressM": 605,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 228719,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537066.491, "y": 6984436.305, "z": 114.97000000000116}, {
        "x": 537059.497,
        "y": 6984480.9,
        "z": 113.94100000000617
      }, {"x": 537056.7, "y": 6984498.342, "z": 113.47400000000198}],
      "id": 228719,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 668,
      "endMValue": 62.805,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023091065,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172551,
      "startAddressM": 1661,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 246674,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536896.257, "y": 6985473.784, "z": 98.28500000000349}, {
        "x": 536847.26,
        "y": 6985569.438,
        "z": 99.22500000000582
      }, {"x": 536820.212, "y": 6985607.936, "z": 99.02700000000186}],
      "id": 246674,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1816,
      "endMValue": 154.523,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827723,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174517,
      "startAddressM": 1067,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 224493,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536990.845, "y": 6984890.152, "z": 97.05100000000675}, {
        "x": 536986.773,
        "y": 6984939.245,
        "z": 95.57600000000093
      }, {"x": 536986.6610047398, "y": 6984941.7328947075, "z": 95.50200313168067}],
      "id": 224493,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1119,
      "endMValue": 51.752,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318831252,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172550,
      "startAddressM": 1886,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 208093,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536780.138, "y": 6985664.499, "z": 99.42600000000675}, {
        "x": 536763.669,
        "y": 6985687.211,
        "z": 99.4829999999929
      }, {"x": 536746.3852264888, "y": 6985712.319670973, "z": 99.54399920065283}],
      "id": 208093,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1945,
      "endMValue": 58.537,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827729,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5174522,
      "startAddressM": 1059,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 243224,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536992.412, "y": 6984881.766, "z": 97.36699999999837}, {
        "x": 536991.913,
        "y": 6984884.513,
        "z": 97.26799999999639
      }, {"x": 536990.953, "y": 6984887.945, "z": 97.15099999999802}, {
        "x": 536990.845016266,
        "y": 6984890.1516676,
        "z": 97.05101506116765
      }],
      "id": 243224,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1067,
      "endMValue": 8.565,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 492848871,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "12.02.2016 10:55:04",
      "linkId": 5174520,
      "startAddressM": 977,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 215819,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537007.428, "y": 6984801.382, "z": 100.26300000000629}, {
        "x": 537004.031,
        "y": 6984817.578,
        "z": 99.66300000000047
      }, {"x": 536999.751, "y": 6984840.12, "z": 98.78599999999278}, {
        "x": 536995.48,
        "y": 6984864.131,
        "z": 97.9829999999929
      }, {"x": 536992.412, "y": 6984881.766, "z": 97.36699999999837}],
      "id": 215819,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1059,
      "endMValue": 81.781,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318831246,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174531,
      "startAddressM": 668,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 234111,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537056.7, "y": 6984498.342, "z": 113.47400000000198}, {
        "x": 537046.658,
        "y": 6984560.725,
        "z": 111.8289999999979
      }, {"x": 537046.5850536687, "y": 6984561.148688281, "z": 111.81600955744628}],
      "id": 234111,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 732,
      "endMValue": 63.616,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1576435606,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172548,
      "startAddressM": 1945,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 266840,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536746.385, "y": 6985712.32, "z": 99.54399999999441}, {
        "x": 536731.02,
        "y": 6985734.129,
        "z": 99.65499999999884
      }, {"x": 536723.424, "y": 6985746.7, "z": 99.81399999999849}],
      "id": 266840,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1986,
      "endMValue": 41.366,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318827963,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172495,
      "startAddressM": 2404,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 189000,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536627.164, "y": 6986147.599, "z": 97.88800000000629}, {
        "x": 536626.891,
        "y": 6986149.589,
        "z": 97.84399999999732
      }, {"x": 536617.598, "y": 6986211.706, "z": 97.23500000000058}, {
        "x": 536617.238030432,
        "y": 6986213.338861956,
        "z": 97.2280005917364
      }],
      "id": 189000,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2470,
      "endMValue": 66.489,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 534655288,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "03.09.2016 22:29:28",
      "linkId": 5172452,
      "startAddressM": 2780,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 207213,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536551.324, "y": 6986512.083, "z": 102.05400000000373}, {
        "x": 536538.097,
        "y": 6986523.848,
        "z": 102.52999999999884
      }, {"x": 536520.003, "y": 6986536.872, "z": 103.2960000000021}, {
        "x": 536505.97,
        "y": 6986546.217,
        "z": 103.81200000000536
      }, {"x": 536489.544, "y": 6986556.34, "z": 104.5109999999986}, {
        "x": 536475.6470053081,
        "y": 6986568.344995414,
        "z": 105.04599979565502
      }],
      "id": 207213,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2875,
      "endMValue": 94.515,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 536475.6528988243, "y": 6986568.339904267, "z": 105.04577290991088},
        "value": 2875
      }],
      "mmlId": 318828227,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 1,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172544,
      "startAddressM": 2028,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 194133,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536701.194, "y": 6985781.618, "z": 100.3350000000064}, {
        "x": 536691.9,
        "y": 6985802.959,
        "z": 100.58599999999569
      }, {"x": 536682.091, "y": 6985830.056, "z": 100.79799999999523}, {
        "x": 536674.439,
        "y": 6985855.135,
        "z": 101.00599999999395
      }, {"x": 536666.272, "y": 6985896.185, "z": 101.27099999999336}],
      "id": 194133,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2148,
      "endMValue": 120.17,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828113,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172454,
      "startAddressM": 2688,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 225351,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536584.363, "y": 6986427.666, "z": 100.8179999999993}, {
        "x": 536581.459,
        "y": 6986444.035,
        "z": 100.95500000000175
      }, {"x": 536576.045, "y": 6986466.432, "z": 101.02300000000105}, {
        "x": 536569.45,
        "y": 6986484.454,
        "z": 101.32200000000012
      }, {"x": 536558.063, "y": 6986503.124, "z": 101.74899999999616}],
      "id": 225351,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2769,
      "endMValue": 80.726,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 492848839,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "03.09.2016 22:29:28",
      "linkId": 5172468,
      "startAddressM": 2769,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 187687,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 536558.063, "y": 6986503.124, "z": 101.74899999999616}, {
        "x": 536557.748,
        "y": 6986503.574,
        "z": 101.7670000000071
      }, {"x": 536552.66, "y": 6986510.554, "z": 102.00599999999395}, {
        "x": 536551.3242289828,
        "y": 6986512.082737938,
        "z": 102.05399177307878
      }],
      "id": 187687,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2780,
      "endMValue": 11.217,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318828221,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5175014,
      "startAddressM": 588,
      "roadNameFi": "Toivalantie",
      "roadPartNumber": 1,
      "administrativeClassMML": "State",
      "segmentId": 207494,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 5653,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 2,
      "points": [{"x": 537069.557, "y": 6984419.497, "z": 114.96700000000419}, {
        "x": 537066.4910629939,
        "y": 6984436.304654663,
        "z": 114.96999993836334
      }],
      "id": 207494,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 605,
      "endMValue": 17.085,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1023091077,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }], [{
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172924,
      "startAddressM": 2348,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 197136,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530927.976, "y": 6985560.371, "z": 107.66999999999825}, {
        "x": 530928.014,
        "y": 6985560.639,
        "z": 107.66099999999278
      }, {"x": 530931.594, "y": 6985578.482, "z": 106.93899999999849}, {
        "x": 530935.703,
        "y": 6985595.674,
        "z": 106.49300000000221
      }, {"x": 530944.151, "y": 6985620.001, "z": 106.17200000000594}, {
        "x": 530955.655,
        "y": 6985646.619,
        "z": 106.26799999999639
      }, {"x": 530966.987, "y": 6985666.436, "z": 106.65600000000268}, {
        "x": 530977.149,
        "y": 6985679.73,
        "z": 106.95299999999406
      }, {"x": 530986.42, "y": 6985688.329, "z": 106.86100000000442}, {
        "x": 530999.588,
        "y": 6985698.859,
        "z": 106.24700000000303
      }, {"x": 531016.977, "y": 6985710.535, "z": 105.14599999999336}, {
        "x": 531038.78,
        "y": 6985725.993,
        "z": 103.75699999999779
      }, {"x": 531060.79, "y": 6985743.347, "z": 102.41700000000128}, {
        "x": 531078.66,
        "y": 6985760.636,
        "z": 101.49800000000687
      }, {"x": 531099.813, "y": 6985784.545, "z": 100.71799999999348}, {
        "x": 531120.15,
        "y": 6985809.128,
        "z": 100.36299999999756
      }, {"x": 531142.547, "y": 6985838.355, "z": 100.07000000000698}, {
        "x": 531166.462,
        "y": 6985870.06,
        "z": 100.51900000000023
      }, {"x": 531183.63, "y": 6985889.269, "z": 100.69999999999709}, {
        "x": 531199.305,
        "y": 6985903.596,
        "z": 100.71499999999651
      }, {"x": 531218.252, "y": 6985916.039, "z": 100.74400000000605}, {
        "x": 531233.374,
        "y": 6985922.961,
        "z": 100.78599999999278
      }, {"x": 531253.997, "y": 6985927.867, "z": 100.66400000000431}, {
        "x": 531285.22,
        "y": 6985929.141,
        "z": 101.11500000000524
      }, {"x": 531318.958, "y": 6985926.707, "z": 101.7390000000014}, {
        "x": 531347.084,
        "y": 6985923.779,
        "z": 101.71700000000419
      }, {"x": 531376.207, "y": 6985923.026, "z": 101.21799999999348}, {
        "x": 531402.354,
        "y": 6985927.301,
        "z": 101.11500000000524
      }, {"x": 531402.735, "y": 6985927.427, "z": 101.11199999999371}],
      "id": 197136,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3005,
      "endMValue": 657.767,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318822238,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.11.2015 14:02:03",
      "linkId": 6479150,
      "startAddressM": 5256,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 192949,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529043.97, "y": 6985067.884, "z": 97.80800000000454}, {
        "x": 529043.727,
        "y": 6985067.975,
        "z": 97.80000000000291
      }, {"x": 529019.965, "y": 6985076.62, "z": 96.93700000000536}, {
        "x": 528973.2,
        "y": 6985089.002,
        "z": 95.23200000000361
      }, {"x": 528967.392, "y": 6985090.439, "z": 95.08299999999872}, {
        "x": 528946.38,
        "y": 6985095.982,
        "z": 94.58100000000559
      }],
      "id": 192949,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5358,
      "endMValue": 101.636,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1016301787,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174263,
      "startAddressM": 3840,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 264507,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530076.47, "y": 6985024.26, "z": 89.41300000000047}, {
        "x": 530077.798,
        "y": 6985026.049,
        "z": 89.46899999999732
      }, {"x": 530099.992, "y": 6985053.465, "z": 90.12900000000081}, {
        "x": 530130.928,
        "y": 6985088.068,
        "z": 90.94400000000314
      }, {"x": 530161.976, "y": 6985120.249, "z": 92.41000000000349}, {
        "x": 530183.876,
        "y": 6985141.379,
        "z": 93.78800000000047
      }, {"x": 530199.847, "y": 6985155.236, "z": 95.24199999999837}, {
        "x": 530218.241,
        "y": 6985167.577,
        "z": 97.16000000000349
      }, {"x": 530239.028, "y": 6985181.028, "z": 99.87799999999697}, {
        "x": 530255.415,
        "y": 6985188.828,
        "z": 101.44800000000396
      }, {"x": 530273.2399821673, "y": 6985194.198994626, "z": 101.84399960382643}],
      "id": 264507,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4104,
      "endMValue": 263.885,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 718328800,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172891,
      "startAddressM": 12,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 222944,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531592.134, "y": 6987984.574, "z": 92.22500000000582}, {
        "x": 531612.998,
        "y": 6988014.744,
        "z": 88.403999999995
      }, {"x": 531634.337, "y": 6988036.948, "z": 85.46799999999348}, {
        "x": 531647.671,
        "y": 6988048.333,
        "z": 84.85599999999977
      }, {"x": 531650.913, "y": 6988050.829, "z": 84.74300000000221}],
      "id": 222944,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 101,
      "endMValue": 89.102,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834554,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174266,
      "startAddressM": 3755,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 243119,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530273.24, "y": 6985194.199, "z": 101.84399999999732}, {
        "x": 530293.323,
        "y": 6985198.967,
        "z": 101.78599999999278
      }, {"x": 530314.337, "y": 6985201.906, "z": 101.94199999999546}, {
        "x": 530357.796,
        "y": 6985207.72,
        "z": 102.60400000000664
      }],
      "id": 243119,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3840,
      "endMValue": 85.706,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318822292,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172965,
      "startAddressM": 837,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 267735,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531629.471, "y": 6987208.902, "z": 106.14500000000407}, {
        "x": 531638.096,
        "y": 6987239.702,
        "z": 104.77999999999884
      }, {"x": 531647.865, "y": 6987278.418, "z": 103.83199999999488}, {
        "x": 531648.208,
        "y": 6987279.821,
        "z": 103.8179999999993
      }],
      "id": 267735,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 910,
      "endMValue": 73.359,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 930547636,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174188,
      "startAddressM": 4147,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 207755,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 529457.574, "y": 6984892.492, "z": 90.05999999999767}, {
        "x": 529476.748,
        "y": 6984886.047,
        "z": 89.64599999999336
      }, {"x": 529499.017, "y": 6984879.404, "z": 89.11699999999837}, {
        "x": 529526.582,
        "y": 6984870.147,
        "z": 88.72699999999895
      }, {"x": 529550.267, "y": 6984862.439, "z": 88.47400000000198}, {
        "x": 529568.395,
        "y": 6984857.436,
        "z": 88.28299999999581
      }, {"x": 529583.723, "y": 6984854.094, "z": 88.08599999999569}, {
        "x": 529604.941,
        "y": 6984849.012,
        "z": 87.83999999999651
      }, {"x": 529634.065, "y": 6984841.385, "z": 87.42999999999302}, {
        "x": 529679.41,
        "y": 6984830.623,
        "z": 86.88999999999942
      }, {"x": 529704.284, "y": 6984826.227, "z": 86.80199999999604}, {
        "x": 529732.78,
        "y": 6984822.478,
        "z": 86.8179999999993
      }, {"x": 529748.32, "y": 6984822.924, "z": 86.74300000000221}, {
        "x": 529760.495,
        "y": 6984823.757,
        "z": 86.73200000000361
      }, {"x": 529773.501, "y": 6984824.647, "z": 86.71199999999953}, {
        "x": 529788.498,
        "y": 6984826.145,
        "z": 86.75400000000081
      }, {"x": 529808.516, "y": 6984831.181, "z": 86.1929999999993}, {
        "x": 529816.523,
        "y": 6984833.199,
        "z": 85.90099999999802
      }, {"x": 529820.641, "y": 6984834.333, "z": 85.77000000000407}, {
        "x": 529840.223,
        "y": 6984840.972,
        "z": 85.19899999999325
      }, {"x": 529860.446, "y": 6984848.308, "z": 84.79200000000128}, {
        "x": 529890.015,
        "y": 6984860.168,
        "z": 84.84200000000419
      }, {"x": 529917.345, "y": 6984872.682, "z": 84.54499999999825}, {
        "x": 529949.448,
        "y": 6984889.861,
        "z": 83.78699999999662
      }, {"x": 529975.811, "y": 6984906.398, "z": 83.7219999999943}, {
        "x": 530000.538,
        "y": 6984924.337,
        "z": 83.97999999999593
      }, {"x": 530021.968, "y": 6984946.383, "z": 85.32799999999406}, {
        "x": 530052.3858671783,
        "y": 6984988.876814448,
        "z": 87.89298879979107
      }],
      "id": 207755,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4805,
      "endMValue": 658.695,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318797351,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174268,
      "startAddressM": 3610,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 204921,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530357.796, "y": 6985207.72, "z": 102.60400000000664}, {
        "x": 530358.365,
        "y": 6985207.796,
        "z": 102.61199999999371
      }, {"x": 530418.501, "y": 6985215.399, "z": 103.54700000000594}, {
        "x": 530446.106,
        "y": 6985220.267,
        "z": 104.25
      }, {"x": 530470.513, "y": 6985226.356, "z": 105.62699999999313}, {
        "x": 530498.9029567405,
        "y": 6985238.010982241,
        "z": 107.05999781644556
      }],
      "id": 204921,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3755,
      "endMValue": 145.064,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318822310,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.11.2015 14:02:03",
      "linkId": 5174204,
      "startAddressM": 4881,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 195534,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529383.984, "y": 6984912.783, "z": 91.11900000000605}, {
        "x": 529359.447,
        "y": 6984918.238,
        "z": 91.34299999999348
      }, {"x": 529327.895, "y": 6984929.001, "z": 92.38400000000547}, {
        "x": 529303.645,
        "y": 6984936.488,
        "z": 93.44899999999325
      }, {"x": 529292.825, "y": 6984941.302, "z": 93.77499999999418}, {
        "x": 529253.985,
        "y": 6984956.846,
        "z": 95.08900000000722
      }, {"x": 529189.313, "y": 6984982.62, "z": 96.88800000000629}, {
        "x": 529149.476,
        "y": 6985002.712,
        "z": 97.33400000000256
      }, {"x": 529105.177, "y": 6985031.299, "z": 97.88300000000163}, {
        "x": 529077.566,
        "y": 6985049.47,
        "z": 97.96199999999953
      }],
      "id": 195534,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5218,
      "endMValue": 337.542,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318798281,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172949,
      "startAddressM": 101,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 203339,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531547.242, "y": 6987773.639, "z": 98.25500000000466}, {
        "x": 531547.231,
        "y": 6987773.839,
        "z": 98.25199999999313
      }, {"x": 531543.366, "y": 6987821.397, "z": 97.00299999999697}, {
        "x": 531542.286,
        "y": 6987860.054,
        "z": 96.43099999999686
      }, {"x": 531544.961, "y": 6987881.702, "z": 95.9829999999929}, {
        "x": 531551.168,
        "y": 6987902.073,
        "z": 95.44100000000617
      }, {"x": 531567.863, "y": 6987940.014, "z": 94.58900000000722}, {
        "x": 531582.195,
        "y": 6987967.64,
        "z": 93.49599999999919
      }, {"x": 531592.134, "y": 6987984.574, "z": 92.22500000000582}],
      "id": 203339,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 323,
      "endMValue": 221.905,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318834548,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.11.2015 14:02:03",
      "linkId": 5174218,
      "startAddressM": 4805,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 255331,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529457.574, "y": 6984892.492, "z": 90.05999999999767}, {
        "x": 529456.203,
        "y": 6984892.953,
        "z": 90.09500000000116
      }, {"x": 529431.984, "y": 6984900.516, "z": 90.53399999999965}, {
        "x": 529411.137,
        "y": 6984905.985,
        "z": 90.89999999999418
      }, {"x": 529397.5612609794, "y": 6984909.43693364, "z": 91.07099671276308}],
      "id": 255331,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4867,
      "endMValue": 62.379,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318797369,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.11.2015 14:02:03",
      "linkId": 5174221,
      "startAddressM": 4867,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 239414,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529397.561, "y": 6984909.437, "z": 91.07099999999627}, {
        "x": 529384.805,
        "y": 6984912.605,
        "z": 91.12900000000081
      }, {"x": 529383.984, "y": 6984912.783, "z": 91.11900000000605}],
      "id": 239414,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4881,
      "endMValue": 13.984,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318798239,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174195,
      "startAddressM": 4104,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 263659,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530052.386, "y": 6984988.877, "z": 87.89299999999639}, {
        "x": 530053.045,
        "y": 6984989.892,
        "z": 87.94800000000396
      }, {"x": 530076.4699354103, "y": 6985024.259905237, "z": 89.41299596056163}],
      "id": 263659,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 4147,
      "endMValue": 42.802,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318824480,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172903,
      "startAddressM": 0,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 227985,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531650.913, "y": 6988050.829, "z": 84.74300000000221}, {
        "x": 531655.526,
        "y": 6988054.415,
        "z": 84.74199999999837
      }, {"x": 531660.3898871768, "y": 6988058.623902369, "z": 84.74199999999837}],
      "id": 227985,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 12,
      "endMValue": 12.275,
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 531660.389887177, "y": 6988058.623902369, "z": 84.74199999999837},
        "value": 0
      }],
      "mmlId": 345368552,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174262,
      "startAddressM": 3323,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 199523,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530498.903, "y": 6985238.011, "z": 107.05999999999767}, {
        "x": 530499.002,
        "y": 6985238.059,
        "z": 107.06399999999849
      }, {"x": 530529.899, "y": 6985254.171, "z": 108.32600000000093}, {
        "x": 530553.464,
        "y": 6985267.84,
        "z": 108.67500000000291
      }, {"x": 530573.801, "y": 6985279.87, "z": 107.13300000000163}, {
        "x": 530592.847,
        "y": 6985289.867,
        "z": 105.23500000000058
      }, {"x": 530609.497, "y": 6985296.458, "z": 104.07000000000698}, {
        "x": 530625.082,
        "y": 6985299.963,
        "z": 103.69199999999546
      }, {"x": 530643.098, "y": 6985301.803, "z": 103.92600000000675}, {
        "x": 530665.594,
        "y": 6985300.189,
        "z": 104.42200000000594
      }, {"x": 530688.205, "y": 6985298.035, "z": 104.88499999999476}, {
        "x": 530711.505,
        "y": 6985299.893,
        "z": 105.39599999999336
      }, {"x": 530727.284, "y": 6985303.96, "z": 105.75500000000466}, {
        "x": 530744.92,
        "y": 6985311.32,
        "z": 106.12399999999616
      }, {"x": 530766.207, "y": 6985323.08, "z": 106.1359999999986}],
      "id": 199523,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3610,
      "endMValue": 287.694,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318822304,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172952,
      "startAddressM": 323,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 212236,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531548.143, "y": 6987761.491, "z": 98.58400000000256}, {
        "x": 531547.2420271544,
        "y": 6987773.638633883,
        "z": 98.25500991545016
      }],
      "id": 212236,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 335,
      "endMValue": 12.181,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 1350065501,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172955,
      "startAddressM": 490,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 223091,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531615.182, "y": 6987516.192, "z": 101.90099999999802}, {
        "x": 531605.658,
        "y": 6987546.37,
        "z": 101.00100000000384
      }, {"x": 531597.38, "y": 6987575.557, "z": 100.43899999999849}, {
        "x": 531587.612,
        "y": 6987611.2,
        "z": 100.13199999999779
      }],
      "id": 223091,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 589,
      "endMValue": 98.941,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832521,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172966,
      "startAddressM": 2107,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 204972,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531402.735, "y": 6985927.427, "z": 101.11199999999371}, {
        "x": 531423.599,
        "y": 6985938.117,
        "z": 100.80499999999302
      }, {"x": 531445.458, "y": 6985956.461, "z": 100.24300000000221}, {
        "x": 531461.881,
        "y": 6985976.969,
        "z": 99.27400000000489
      }, {"x": 531476.934, "y": 6986005.682, "z": 98.19999999999709}, {
        "x": 531483.304,
        "y": 6986032.463,
        "z": 98.27000000000407
      }, {"x": 531486.587, "y": 6986067.193, "z": 98.5170000000071}, {
        "x": 531491.554,
        "y": 6986135.134,
        "z": 99.64299999999639
      }],
      "id": 204972,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2348,
      "endMValue": 241.208,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823306,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5174271,
      "startAddressM": 3181,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 200943,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530766.207, "y": 6985323.08, "z": 106.1359999999986}, {
        "x": 530767.166,
        "y": 6985323.622,
        "z": 106.11500000000524
      }, {"x": 530804.108, "y": 6985342.837, "z": 105.04300000000512}, {
        "x": 530835.37,
        "y": 6985357.666,
        "z": 103.57200000000012
      }, {"x": 530864.633, "y": 6985371.885, "z": 103.83100000000559}, {
        "x": 530879.711,
        "y": 6985382.662,
        "z": 104.3350000000064
      }, {"x": 530889.3508350481, "y": 6985392.111838299, "z": 104.744992984406}],
      "id": 200943,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3323,
      "endMValue": 141.91,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318822256,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172992,
      "startAddressM": 3005,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 214634,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 530889.351, "y": 6985392.112, "z": 104.74499999999534}, {
        "x": 530894.553,
        "y": 6985398.184,
        "z": 105.10599999999977
      }, {"x": 530905.005, "y": 6985413.239, "z": 106.13300000000163}, {
        "x": 530912.983,
        "y": 6985431.788,
        "z": 107.55400000000373
      }, {"x": 530916.632, "y": 6985446.085, "z": 108.36199999999371}, {
        "x": 530919.418,
        "y": 6985462.243,
        "z": 108.72299999999814
      }, {"x": 530923.084, "y": 6985494.859, "z": 108.53699999999662}, {
        "x": 530925.597,
        "y": 6985529.384,
        "z": 108.43099999999686
      }, {"x": 530927.976, "y": 6985560.371, "z": 107.66999999999825}],
      "id": 214634,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 3181,
      "endMValue": 176.183,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 930207186,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172948,
      "startAddressM": 335,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 204403,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531587.612, "y": 6987611.2, "z": 100.13199999999779}, {
        "x": 531579.946,
        "y": 6987639.171,
        "z": 100.0969999999943
      }, {"x": 531570.105, "y": 6987673.266, "z": 99.92799999999988}, {
        "x": 531556.959,
        "y": 6987717.95,
        "z": 99.92699999999604
      }, {"x": 531548.374, "y": 6987758.447, "z": 98.67200000000594}, {
        "x": 531548.143,
        "y": 6987761.491,
        "z": 98.58400000000256
      }],
      "id": 204403,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 490,
      "endMValue": 155.517,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832509,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172944,
      "startAddressM": 589,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 205682,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531648.208, "y": 6987279.821, "z": 103.8179999999993}, {
        "x": 531658.04,
        "y": 6987321.178,
        "z": 104.07700000000477
      }, {"x": 531664.705, "y": 6987350.87, "z": 104.43600000000151}, {
        "x": 531665.69,
        "y": 6987372.073,
        "z": 104.528999999995
      }, {"x": 531662.633, "y": 6987393.604, "z": 104.87699999999313}, {
        "x": 531652.383,
        "y": 6987421.481,
        "z": 105.05199999999604
      }, {"x": 531641.048, "y": 6987444.826, "z": 105.00699999999779}, {
        "x": 531631.434,
        "y": 6987465.359,
        "z": 105.0280000000057
      }, {"x": 531623.545, "y": 6987485.407, "z": 104.05499999999302}, {
        "x": 531617.894,
        "y": 6987505.687,
        "z": 102.55199999999604
      }, {"x": 531615.304, "y": 6987515.758, "z": 101.91899999999441}, {
        "x": 531615.182021248,
        "y": 6987516.191924413,
        "z": 101.90100313494891
      }],
      "id": 205682,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 837,
      "endMValue": 247.685,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832545,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "20.11.2015 14:02:03",
      "linkId": 5174216,
      "startAddressM": 5218,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 252771,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 2,
      "points": [{"x": 529077.566, "y": 6985049.47, "z": 97.96199999999953}, {
        "x": 529077.129,
        "y": 6985049.747,
        "z": 97.95600000000559
      }, {"x": 529043.97, "y": 6985067.884, "z": 97.80800000000454}],
      "id": 252771,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 5256,
      "endMValue": 38.313,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318798290,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172960,
      "startAddressM": 930,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 221837,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531342.47, "y": 6986712.906, "z": 97.35000000000582}, {
        "x": 531351.963,
        "y": 6986731.565,
        "z": 98.54099999999744
      }, {"x": 531366.829, "y": 6986760.238, "z": 100.70299999999406}, {
        "x": 531381.618,
        "y": 6986787.19,
        "z": 103.48699999999371
      }, {"x": 531396.661, "y": 6986811.935, "z": 106.02599999999802}, {
        "x": 531418.985,
        "y": 6986850.765,
        "z": 107.43099999999686
      }, {"x": 531444.085, "y": 6986880.927, "z": 108.64800000000105}, {
        "x": 531469.389,
        "y": 6986905.048,
        "z": 110.278999999995
      }, {"x": 531496.734, "y": 6986930.71, "z": 112.82600000000093}, {
        "x": 531522.818,
        "y": 6986959.278,
        "z": 114.51900000000023
      }, {"x": 531543.963, "y": 6986989.461, "z": 115.76799999999639}, {
        "x": 531554.457,
        "y": 6987006.876,
        "z": 116.54399999999441
      }, {"x": 531578.639, "y": 6987057.168, "z": 112.56500000000233}, {
        "x": 531584.555,
        "y": 6987071.875,
        "z": 111.50599999999395
      }, {"x": 531598.275, "y": 6987108.934, "z": 109.53699999999662}, {
        "x": 531613.495,
        "y": 6987153.805,
        "z": 107.58800000000338
      }, {"x": 531624.474, "y": 6987189.094, "z": 106.5850000000064}],
      "id": 221837,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 1490,
      "endMValue": 560.806,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318823102,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172969,
      "startAddressM": 1490,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 242036,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531491.554, "y": 6986135.134, "z": 99.64299999999639}, {
        "x": 531491.625,
        "y": 6986136.101,
        "z": 99.66000000000349
      }, {"x": 531495.671, "y": 6986171.415, "z": 100.57399999999325}, {
        "x": 531502.191,
        "y": 6986215.741,
        "z": 102.03500000000349
      }, {"x": 531504.021, "y": 6986255.547, "z": 103.38700000000244}, {
        "x": 531497.918,
        "y": 6986291.792,
        "z": 104.8859999999986
      }, {"x": 531495.3, "y": 6986300.853, "z": 105.11699999999837}, {
        "x": 531487.943,
        "y": 6986322.51,
        "z": 105.21199999999953
      }, {"x": 531478.89, "y": 6986347.016, "z": 104.26300000000629}, {
        "x": 531468.967,
        "y": 6986376.192,
        "z": 101.71700000000419
      }, {"x": 531456.372, "y": 6986411.66, "z": 98.44999999999709}, {
        "x": 531441.172,
        "y": 6986447.87,
        "z": 96.52300000000105
      }, {"x": 531429.078, "y": 6986470.798, "z": 96.4429999999993}, {
        "x": 531414.309,
        "y": 6986493.823,
        "z": 96.89999999999418
      }, {"x": 531397.551, "y": 6986518.476, "z": 97.38099999999395}, {
        "x": 531379.462,
        "y": 6986545.5,
        "z": 97.25500000000466
      }, {"x": 531365.485, "y": 6986572.328, "z": 97.83100000000559}, {
        "x": 531346.979,
        "y": 6986612.348,
        "z": 98.72400000000198
      }, {"x": 531338.012, "y": 6986638.789, "z": 98.00900000000547}, {
        "x": 531333.657,
        "y": 6986661.166,
        "z": 97.0399999999936
      }, {"x": 531333.514, "y": 6986676.71, "z": 96.4600000000064}, {
        "x": 531335.807,
        "y": 6986693.379,
        "z": 96.60000000000582
      }, {"x": 531342.287, "y": 6986712.5, "z": 97.3350000000064}, {
        "x": 531342.47,
        "y": 6986712.906,
        "z": 97.35000000000582
      }],
      "id": 242036,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 2107,
      "endMValue": 617.746,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 718328725,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }, {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 5172962,
      "startAddressM": 910,
      "roadNameFi": "Kehvontie",
      "roadPartNumber": 3,
      "administrativeClassMML": "State",
      "segmentId": 189576,
      "municipalityCode": 749,
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 16279,
      "trackCode": 0,
      "roadClass": 5,
      "sideCode": 3,
      "points": [{"x": 531624.474, "y": 6987189.094, "z": 106.5850000000064}, {
        "x": 531624.675,
        "y": 6987189.775,
        "z": 106.58100000000559
      }, {"x": 531629.349, "y": 6987208.414, "z": 106.1530000000057}, {
        "x": 531629.4709599594,
        "y": 6987208.901839837,
        "z": 106.14500262561695
      }],
      "id": 189576,
      "administrativeClassId": "1",
      "status": 99,
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 930,
      "endMValue": 20.429,
      "linkType": 99,
      "calibrationPoints": [],
      "mmlId": 318832563,
      "modifiedBy": "vvh_modified",
      "elyCode": 8,
      "discontinuity": 5,
      "roadLinkSource": 1
    }
    ]];
  };

  var generateTerminatedProjectLinkData = function () {
    return [
      [
        {
          "modifiedAt": "12.02.2016 10:55:04",
          "linkId": 5172091,
          "startAddressM": 734,
          "roadNameFi": "VT 5",
          "roadPartNumber": 205,
          "administrativeClassMML": "State",
          "segmentId": 454340,
          "municipalityCode": 749,
          "roadLinkType": 1,
          "constructionType": 0,
          "roadNumber": 5,
          "trackCode": 1,
          "roadClass": 1,
          "sideCode": 2,
          "points": [{"x": 533350.231, "y": 6988423.725, "z": 112.24599999999919}, {
            "x": 533341.188,
            "y": 6988460.23,
            "z": 112.53699999999662
          }, {"x": 533327.561, "y": 6988514.938, "z": 113.028999999995}, {
            "x": 533316.53,
            "y": 6988561.507,
            "z": 113.4149999999936
          }, {"x": 533299.757, "y": 6988642.994, "z": 113.67600000000675}, {
            "x": 533284.518,
            "y": 6988725.655,
            "z": 113.58000000000175
          }, {"x": 533270.822, "y": 6988809.254, "z": 113.16300000000047}, {
            "x": 533259.436,
            "y": 6988892.555,
            "z": 112.38000000000466
          }, {"x": 533249.153, "y": 6988975.982, "z": 111.625}, {
            "x": 533245.308,
            "y": 6989012.096,
            "z": 111.29899999999907
          }, {"x": 533244.289, "y": 6989018.034, "z": 111.36000000000058}],
          "id": 454340,
          "administrativeClassId": "1",
          "status": 1,
          "anomaly": 0,
          "startMValue": 0.0,
          "endAddressM": 1340,
          "endMValue": 604.2852198046263,
          "linkType": 99,
          "calibrationPoints": [],
          "mmlId": 318834500,
          "modifiedBy": "vvh_modified",
          "elyCode": 8,
          "discontinuity": 5,
          "roadLinkSource": 1
        }
      ]
    ];
  };

  var generateProjectLinksByProjectId = function () {
    return {
      "project": {
        "statusCode": 1,
        "name": "Project Two",
        "statusDescription": "Keskeneräinen",
        "dateModified": "01.06.2017",
        "id": 454808,
        "createdBy": "silari",
        "additionalInfo": null,
        "startDate": "01.06.2017",
        "modifiedBy": "-",
        "createdDate": "2017-06-01T19:03:12.000+03:00"
      },
      "linkId": 1717275,
      "projectLinks": [{
        "startingLinkId": 1717275,
        "projectId": 454808,
        "roadNumber": 1130,
        "roadPartNumber": 4,
        "roadLength": 6362,
        "ely": 1,
        "discontinuity": "Jatkuva"
      }],
      "projectErrors": []
    };
  };

  var generateRoadPartChecker = function () {
    return {
      "success": "ok",
      "roadparts": [{
        "roadPartNumber": 4,
        "roadNumber": 1130,
        "ely": 1,
        "length": 6362.0,
        "roadPartId": 34320,
        "discontinuity": "Jatkuva"
      }]
    };
  };

  var generateSaveRoadAddressProject = function () {
    return {
      "project": {
        "name": "Project Two",
        "dateModified": "01.06.2017",
        "id": 454604,
        "status": {},
        "createdBy": "silari",
        "additionalInfo": "",
        "startDate": "01.06.2017",
        "modifiedBy": "-"
      },
      "projectAddresses": {
        "id": 454605,
        "roadNumber": 1130,
        "roadPartNumber": 4,
        "track": {},
        "discontinuity": "Jatkuva",
        "startAddrMValue": 0,
        "endAddrMValue": 17,
        "linkId": 1717275,
        "startMValue": 0.0,
        "endMValue": 16.0,
        "sideCode": {},
        "calibrationPoints": {},
        "geom": [],
        "projectId": 454604,
        "status": {}
      },
      "reservedInfo": [{
        "startingLinkId": 1717275,
        "projectId": 454604,
        "roadNumber": 1130,
        "roadPartNumber": 4,
        "roadLength": 6362,
        "ely": 1,
        "discontinuity": "Jatkuva"
      }],
      "success": "ok"
    };
  };

  var generateRoadLinkByLinkId = function () {
    return {
      "modifiedAt": "29.10.2015 15:34:02",
      "linkId": 1717275,
      "startAddressM": 0,
      "roadNameFi": "Evitskogintie",
      "roadPartNumber": 4,
      "endDate": "",
      "administrativeClassMML": "State",
      "segmentId": 57023,
      "municipalityCode": 257,
      "middlePoint": {"x": 359548.14991305507, "y": 6679203.066105741, "z": 55.032556928226455},
      "roadLinkType": 1,
      "constructionType": 0,
      "roadNumber": 1130,
      "trackCode": 0,
      "roadClass": 4,
      "sideCode": 3,
      "points": [{"x": 359540.682, "y": 6679199.184, "z": 55.00100000000384}, {
        "x": 359542.395,
        "y": 6679200.065,
        "z": 55.070000000006985
      }, {"x": 359555.613, "y": 6679206.958, "z": 54.98399999999674}],
      "id": 57023,
      "administrativeClassId": "1",
      "anomaly": 0,
      "startMValue": 0.0,
      "endAddressM": 17,
      "endMValue": 16.834,
      "roadNameSe": "Evitskogsvägen",
      "linkType": 99,
      "calibrationPoints": [{
        "point": {"x": 359555.6131170889, "y": 6679206.95806106, "z": 54.98399923818352},
        "value": 0
      }],
      "mmlId": 362905203,
      "startDate": "01.01.1996",
      "modifiedBy": "vvh_modified",
      "elyCode": 1,
      "discontinuity": 5,
      "roadLinkSource": 1
    };
  };

  var generateCreateRoadAddressProject = function () {
    return {
      "project": {
        "name": "Project Two",
        "dateModified": "01.06.2017",
        "id": 454756,
        "status": {},
        "createdBy": "silari",
        "additionalInfo": "",
        "startDate": "01.06.2017",
        "modifiedBy": "-"
      },
      "projectAddresses": {
        "id": 454757,
        "roadNumber": 1130,
        "roadPartNumber": 4,
        "track": {},
        "discontinuity": "Jatkuva",
        "startAddrMValue": 0,
        "endAddrMValue": 17,
        "linkId": 1717275,
        "startMValue": 0.0,
        "endMValue": 16.0,
        "sideCode": {},
        "calibrationPoints": {},
        "geom": [],
        "projectId": 454756,
        "status": {}
      },
      "reservedInfo": [{
        "startingLinkId": 1718137,
        "projectId": 454756,
        "roadNumber": 1130,
        "roadPartNumber": 4,
        "roadLength": 6362,
        "ely": 1,
        "discontinuity": "Jatkuva"
      }],
      "success": "ok"
    };
  };

  root.RoadAddressProjectTestData = {
    generate: generate,
    generateNormalLinkData: generateNormalLinkData,
    generateProjectLinkData: generateProjectLinkData,
    generateTerminatedProjectLinkData: generateTerminatedProjectLinkData,
    generateProjectLinksByProjectId: generateProjectLinksByProjectId,
    generateRoadPartChecker: generateRoadPartChecker,
    generateProject: generateProject,
    generateRoadLinkByLinkId: generateRoadLinkByLinkId,
    generateCreateRoadAddressProject: generateCreateRoadAddressProject,
    generateProjectLinks: generateProjectLinks
  };
}(this));
