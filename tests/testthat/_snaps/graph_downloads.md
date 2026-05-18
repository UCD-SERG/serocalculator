# .aggregate_by_unit() aggregates correctly

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["date", "provider", "new", "cumulative"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [19723, 19754]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["CRAN", "CRAN"]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [60, 20]
        },
        {
          "type": "integer",
          "attributes": {},
          "value": [60, 80]
        }
      ]
    }

# .prepare_download_data() pivots correctly

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["date", "provider", "metric", "downloads"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [19905, 19905, 19936, 19936, 19967, 19967, 19997, 19997, 20028, 20028, 20058, 20058, 20089, 20089, 20120, 20120, 20148, 20148, 20179, 20179, 20209, 20209, 20240, 20240, 20270, 20270, 20301, 20301, 20332, 20332, 20362, 20362, 20393, 20393, 20423, 20423, 20454, 20454, 20485, 20485, 20513, 20513, 20544, 20544]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["New downloads", "Cumulative downloads"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [114, 114, 132, 246, 161, 407, 100, 507, 108, 615, 8, 623, 96, 719, 336, 1055, 129, 1184, 161, 1345, 117, 1462, 139, 1601, 165, 1766, 143, 1909, 185, 2094, 210, 2304, 133, 2437, 12523, 14960, 7122, 22082, 28, 22110, 4324, 26434, 9576, 36010]
        }
      ]
    }

# .prepare_download_data() filters by start date

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["date", "provider", "metric", "downloads"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [20454, 20485, 20513, 20544]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["CRAN", "CRAN", "CRAN", "CRAN"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["New downloads"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [7122, 28, 4324, 9576]
        }
      ]
    }

# .prepare_download_data() includes only requested metrics

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["date", "provider", "metric", "downloads"]
        },
        "row.names": {
          "type": "integer",
          "attributes": {},
          "value": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22]
        },
        "class": {
          "type": "character",
          "attributes": {},
          "value": ["tbl_df", "tbl", "data.frame"]
        }
      },
      "value": [
        {
          "type": "double",
          "attributes": {
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["Date"]
            }
          },
          "value": [19905, 19936, 19967, 19997, 20028, 20058, 20089, 20120, 20148, 20179, 20209, 20240, 20270, 20301, 20332, 20362, 20393, 20423, 20454, 20485, 20513, 20544]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN", "CRAN"]
        },
        {
          "type": "integer",
          "attributes": {
            "levels": {
              "type": "character",
              "attributes": {},
              "value": ["New downloads"]
            },
            "class": {
              "type": "character",
              "attributes": {},
              "value": ["factor"]
            }
          },
          "value": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        },
        {
          "type": "double",
          "attributes": {},
          "value": [114, 132, 161, 100, 108, 8, 96, 336, 129, 161, 117, 139, 165, 143, 185, 210, 133, 12523, 7122, 28, 4324, 9576]
        }
      ]
    }

