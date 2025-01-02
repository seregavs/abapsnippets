# SAP Enhancement RSAP0001
(Tcode CMOD)
+ EXIT_SAPLRSAP_001   Transaction data
+ EXIT_SAPLRSAP_002   Attributes, texts
+ EXIT_SAPLRSAP_003   Texts
+ EXIT_SAPLRSAP_004   Hierarchies<br>

(https://community.sap.com/t5/technology-blogs-by-members/enhancement-of-bw-datasources-using-badi/ba-p/13085219)

# SAP BAdI for extractor extension RSU5_SAPI_BADI
(Tcode SE19)
+ DATA_TRANSFORM    Method for General Data Transfer
+ HIER_TRANSFORM    Method for Hierarchy Data Transfer<br>

(https://community.sap.com/t5/technology-blogs-by-members/enhancement-of-bw-datasources-using-badi/ba-p/13085219)<br>
(https://community.sap.com/t5/enterprise-resource-planning-blogs-by-members/badi-implementation-in-abap-on-hana-for-bw-extractors/ba-p/13510189)<br>
(https://community.sap.com/t5/technology-blogs-by-members/implementation-of-badi-enhancing-0asset-attr/ba-p/13302933)

# SAP BAdI for ODP-extractors BADI_RODPS_DATASOURCE_EXT
(TCode SE19) New BAdI from enh spot ODPS_DATASOURCE_EXT<br>
ODP-extractors support both BADIs<br>
(https://help.sap.com/doc/saphelp_nw73ehp1/7.31.19/en-US/8a/4121d239094ffebac228e05771cb82/content.htm)<br>
(https://help.sap.com/docs/SAP_BW4HANA/107a6e8a38b74ede94c833ca3b7b6f51/7b5bce89037f4897a69b02ecda028517.html)

# SAP BAdI for user-exit variables RSROA_VARIABLES_EXIT_BADI
(Tcode SE19) New BAdi from enh.spot RSROA_VARIABLES_EXIT
+ PROCESS<br>

(https://community.sap.com/t5/technology-blogs-by-members/customer-exit-variable-implementation-using-badi/ba-p/13535137)

# SAP BAdI for F4-help RSR_VARIABLE_F4_RESTRICT_BADI
(Tcode SE19) New BAdI from enh spot RSR_VARIABLE_F4_RESTRICT<br>
+ IF_RSR_VARIABLE_F4_RESTRICT~GET_RESTRICTION_FLAT  -  to restrict F4 help for value variables
+ IF_RSR_VARIABLE_F4_RESTRICT~GET_RESTRICTION_NODE  -  to restrict F4 help for ´hierarchy node variables
+ IF_RSR_VARIABLE_F4_RESTRICT~GET_RESTRICTION_HIER  -  to restrict F4 help for hierarchy variables<br>

(https://help.sap.com/docs/SUPPORT_CONTENT/bwplaolap/3361386039.html)

# SAP BAdI for decision in process chain RSAR_CONNECTOR
(Tcode SE19)<br>
(https://community.sap.com/t5/technology-blogs-by-members/how-to-better-decision-in-process-chain-by-using-custom-formulas/ba-p/13218478)

# SAP BAdI for virtual ky figures and characteristics RSROA_OLAP_BADI
(Tcode SE19) New BAdI from enh spot RSROA<br>
old version is RSR_OLAP_BADI
+ DEFINE - This method is only called during generation of the query
+ INITIALIZE - determines the position (corresponding instance attributes)of the key figures/characteristics in the structure C_S_DATA. Called once for each read request
+ COMPUTE is available in two versions:
  + COMPUTE_SINGLE - process single record by record
  + COMPUTE_TABLE - process whole datapackage<br>

(https://help.sap.com/docs/SUPPORT_CONTENT/bwplaolap/3361383939.html)<br>
(https://help.sap.com/docs/SUPPORT_CONTENT/bwplaolap/3361385718.html)

# SAP BAdI for virtual analysis authorization RSEC_VIRTUAL_AUTH_BADI
(Tcode SE19) New BAdi from enh.spot RSEC_VIRTUAL_AUTH<br>
(https://community.sap.com/t5/technology-blogs-by-members/implementing-virtual-analysis-authorizations/ba-p/13275567)

# SAP BAdI for RRI-jump with changing parameteres RS_BBS_BADI
(Tcode SE19) New classic BADI from RS_BBS_BADI<br>
(https://community.sap.com/t5/technology-blogs-by-members/report-to-report-interface-rri/ba-p/13366601)

---

# SAP BAdI data provider RSO_BADI_PROVIDER
not recommended to use<br>
(Tcode SE19)<br>
(https://help.sap.com/docs/SAP_BW4HANA/107a6e8a38b74ede94c833ca3b7b6f51/9d157e8996714771bb11c1fab34a6412.html)

