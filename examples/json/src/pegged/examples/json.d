module pegged.examples.json;

import pegged.grammar;

@safe:

mixin(grammar(`
JSON:
    JSONObject <  :'{' (Pair (:',' Pair)*)? :'}'
    Pair       <  String :':' Value
    Array      <  :'[' (Value (:',' Value)* )? :']'

    Value  <  String
            / Number
            / JSONObject
            / Array
            / True
            / False
            / Null
    True   <- "true"
    False  <- "false"
    Null   <- "null"

    String <~ :doublequote Char* :doublequote
    Char   <~ backslash doublequote
            / backslash backslash
            / backslash [bfnrt]
            / backslash 'u' Hex Hex Hex Hex
            / (!doublequote .)

    Number <~ '0'
            / [1-9] Digit* ('.' Digit*)?
    Digit  <- [0-9]
    Hex    <- [0-9A-Fa-f]
`));

unittest
{
    enum example2 = `
    {
    "Number": 42,
    "Decimal": 123.456,
    "String": "abc",
    "NullString": "",
    "Escape": "\uAAAA\n\\Hello",
    "Array" : [0,1,2],
    "Array2": [0, [0,1,2], "abc"],
    "Obj"   : { "Member":0, "Member":[0,1,2] },
    "True"  : true,
    "False" : false,
    "Null"  : null,
    "Empty" : {}
    }`;

    const example2Tree = JSON(example2);
    assert(example2Tree.successful);
    assert(example2Tree[0].children.length == 12);
    assert(example2Tree[0][0][0].matches == ["Number"]);
    assert(example2Tree[0][0][1].matches == ["42"]);
    assert(example2Tree.toString == q"EOS
JSON[0, 314]["Number", "42", "Decimal", "123.456", "String", "abc", "NullString", "Escape", "\\uAAAA\\n\\\\Hello", "Array", "0", "1", "2", "Array2", "0", "0", "1", "2", "abc", "Obj", "Member", "0", "Member", "0", "1", "2", "True", "true", "False", "false", "Null", "null", "Empty"]
 +-JSON.JSONObject[0, 314]["Number", "42", "Decimal", "123.456", "String", "abc", "NullString", "Escape", "\\uAAAA\\n\\\\Hello", "Array", "0", "1", "2", "Array2", "0", "0", "1", "2", "abc", "Obj", "Member", "0", "Member", "0", "1", "2", "True", "true", "False", "false", "Null", "null", "Empty"]
    +-JSON.Pair[11, 23]["Number", "42"]
    |  +-JSON.String[11, 19]["Number"]
    |  +-JSON.Value[21, 23]["42"]
    |     +-JSON.Number[21, 23]["42"]
    +-JSON.Pair[29, 47]["Decimal", "123.456"]
    |  +-JSON.String[29, 38]["Decimal"]
    |  +-JSON.Value[40, 47]["123.456"]
    |     +-JSON.Number[40, 47]["123.456"]
    +-JSON.Pair[53, 68]["String", "abc"]
    |  +-JSON.String[53, 61]["String"]
    |  +-JSON.Value[63, 68]["abc"]
    |     +-JSON.String[63, 68]["abc"]
    +-JSON.Pair[74, 90]["NullString"]
    |  +-JSON.String[74, 86]["NullString"]
    +-JSON.Pair[96, 123]["Escape", "\\uAAAA\\n\\\\Hello"]
    |  +-JSON.String[96, 104]["Escape"]
    |  +-JSON.Value[106, 123]["\\uAAAA\\n\\\\Hello"]
    |     +-JSON.String[106, 123]["\\uAAAA\\n\\\\Hello"]
    +-JSON.Pair[129, 146]["Array", "0", "1", "2"]
    |  +-JSON.String[129, 137]["Array"]
    |  +-JSON.Value[139, 146]["0", "1", "2"]
    |     +-JSON.Array[139, 146]["0", "1", "2"]
    |        +-JSON.Value[140, 141]["0"]
    |        |  +-JSON.Number[140, 141]["0"]
    |        +-JSON.Value[142, 143]["1"]
    |        |  +-JSON.Number[142, 143]["1"]
    |        +-JSON.Value[144, 145]["2"]
    |           +-JSON.Number[144, 145]["2"]
    +-JSON.Pair[152, 181]["Array2", "0", "0", "1", "2", "abc"]
    |  +-JSON.String[152, 160]["Array2"]
    |  +-JSON.Value[162, 181]["0", "0", "1", "2", "abc"]
    |     +-JSON.Array[162, 181]["0", "0", "1", "2", "abc"]
    |        +-JSON.Value[163, 164]["0"]
    |        |  +-JSON.Number[163, 164]["0"]
    |        +-JSON.Value[166, 173]["0", "1", "2"]
    |        |  +-JSON.Array[166, 173]["0", "1", "2"]
    |        |     +-JSON.Value[167, 168]["0"]
    |        |     |  +-JSON.Number[167, 168]["0"]
    |        |     +-JSON.Value[169, 170]["1"]
    |        |     |  +-JSON.Number[169, 170]["1"]
    |        |     +-JSON.Value[171, 172]["2"]
    |        |        +-JSON.Number[171, 172]["2"]
    |        +-JSON.Value[175, 180]["abc"]
    |           +-JSON.String[175, 180]["abc"]
    +-JSON.Pair[187, 229]["Obj", "Member", "0", "Member", "0", "1", "2"]
    |  +-JSON.String[187, 195]["Obj"]
    |  +-JSON.Value[197, 229]["Member", "0", "Member", "0", "1", "2"]
    |     +-JSON.JSONObject[197, 229]["Member", "0", "Member", "0", "1", "2"]
    |        +-JSON.Pair[199, 209]["Member", "0"]
    |        |  +-JSON.String[199, 207]["Member"]
    |        |  +-JSON.Value[208, 209]["0"]
    |        |     +-JSON.Number[208, 209]["0"]
    |        +-JSON.Pair[211, 228]["Member", "0", "1", "2"]
    |           +-JSON.String[211, 219]["Member"]
    |           +-JSON.Value[220, 228]["0", "1", "2"]
    |              +-JSON.Array[220, 228]["0", "1", "2"]
    |                 +-JSON.Value[221, 222]["0"]
    |                 |  +-JSON.Number[221, 222]["0"]
    |                 +-JSON.Value[223, 224]["1"]
    |                 |  +-JSON.Number[223, 224]["1"]
    |                 +-JSON.Value[225, 226]["2"]
    |                    +-JSON.Number[225, 226]["2"]
    +-JSON.Pair[235, 249]["True", "true"]
    |  +-JSON.String[235, 243]["True"]
    |  +-JSON.Value[245, 249]["true"]
    |     +-JSON.True[245, 249]["true"]
    +-JSON.Pair[255, 270]["False", "false"]
    |  +-JSON.String[255, 263]["False"]
    |  +-JSON.Value[265, 270]["false"]
    |     +-JSON.False[265, 270]["false"]
    +-JSON.Pair[276, 290]["Null", "null"]
    |  +-JSON.String[276, 284]["Null"]
    |  +-JSON.Value[286, 290]["null"]
    |     +-JSON.Null[286, 290]["null"]
    +-JSON.Pair[296, 313]["Empty"]
       +-JSON.String[296, 304]["Empty"]
EOS");

    enum example3 =
        `{
        "glossary": {
            "title": "example glossary",
            "GlossDiv": {
                "title": "S",
                "GlossList": {
                    "GlossEntry": {
                        "ID": "SGML",
                        "SortAs": "SGML",
                        "GlossTerm": "Standard Generalized Markup Language",
                        "Acronym": "SGML",
                        "Abbrev": "ISO 8879:1986",
                        "GlossDef": {
                            "para": "A meta-markup language, used to create markup languages such as DocBook.",
                            "GlossSeeAlso": ["GML", "XML"]
                        },
                        "GlossSee": "markup"
                    }
                }
            }
        }
    }`;
    assert(JSON(example3).successful);
    assert(JSON(example3).toString == q"EOS
JSON[0, 789]["glossary", "title", "example glossary", "GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
 +-JSON.JSONObject[0, 789]["glossary", "title", "example glossary", "GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
    +-JSON.Pair[10, 788]["glossary", "title", "example glossary", "GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
       +-JSON.String[10, 20]["glossary"]
       +-JSON.Value[22, 788]["title", "example glossary", "GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
          +-JSON.JSONObject[22, 788]["title", "example glossary", "GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
             +-JSON.Pair[36, 63]["title", "example glossary"]
             |  +-JSON.String[36, 43]["title"]
             |  +-JSON.Value[45, 63]["example glossary"]
             |     +-JSON.String[45, 63]["example glossary"]
             +-JSON.Pair[77, 782]["GlossDiv", "title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                +-JSON.String[77, 87]["GlossDiv"]
                +-JSON.Value[89, 782]["title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                   +-JSON.JSONObject[89, 782]["title", "S", "GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                      +-JSON.Pair[107, 119]["title", "S"]
                      |  +-JSON.String[107, 114]["title"]
                      |  +-JSON.Value[116, 119]["S"]
                      |     +-JSON.String[116, 119]["S"]
                      +-JSON.Pair[137, 772]["GlossList", "GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                         +-JSON.String[137, 148]["GlossList"]
                         +-JSON.Value[150, 772]["GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                            +-JSON.JSONObject[150, 772]["GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                               +-JSON.Pair[172, 758]["GlossEntry", "ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                                  +-JSON.String[172, 184]["GlossEntry"]
                                  +-JSON.Value[186, 758]["ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                                     +-JSON.JSONObject[186, 758]["ID", "SGML", "SortAs", "SGML", "GlossTerm", "Standard Generalized Markup Language", "Acronym", "SGML", "Abbrev", "ISO 8879:1986", "GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML", "GlossSee", "markup"]
                                        +-JSON.Pair[212, 224]["ID", "SGML"]
                                        |  +-JSON.String[212, 216]["ID"]
                                        |  +-JSON.Value[218, 224]["SGML"]
                                        |     +-JSON.String[218, 224]["SGML"]
                                        +-JSON.Pair[250, 266]["SortAs", "SGML"]
                                        |  +-JSON.String[250, 258]["SortAs"]
                                        |  +-JSON.Value[260, 266]["SGML"]
                                        |     +-JSON.String[260, 266]["SGML"]
                                        +-JSON.Pair[292, 343]["GlossTerm", "Standard Generalized Markup Language"]
                                        |  +-JSON.String[292, 303]["GlossTerm"]
                                        |  +-JSON.Value[305, 343]["Standard Generalized Markup Language"]
                                        |     +-JSON.String[305, 343]["Standard Generalized Markup Language"]
                                        +-JSON.Pair[369, 386]["Acronym", "SGML"]
                                        |  +-JSON.String[369, 378]["Acronym"]
                                        |  +-JSON.Value[380, 386]["SGML"]
                                        |     +-JSON.String[380, 386]["SGML"]
                                        +-JSON.Pair[412, 437]["Abbrev", "ISO 8879:1986"]
                                        |  +-JSON.String[412, 420]["Abbrev"]
                                        |  +-JSON.Value[422, 437]["ISO 8879:1986"]
                                        |     +-JSON.String[422, 437]["ISO 8879:1986"]
                                        +-JSON.Pair[463, 673]["GlossDef", "para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML"]
                                        |  +-JSON.String[463, 473]["GlossDef"]
                                        |  +-JSON.Value[475, 673]["para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML"]
                                        |     +-JSON.JSONObject[475, 673]["para", "A meta-markup language, used to create markup languages such as DocBook.", "GlossSeeAlso", "GML", "XML"]
                                        |        +-JSON.Pair[505, 587]["para", "A meta-markup language, used to create markup languages such as DocBook."]
                                        |        |  +-JSON.String[505, 511]["para"]
                                        |        |  +-JSON.Value[513, 587]["A meta-markup language, used to create markup languages such as DocBook."]
                                        |        |     +-JSON.String[513, 587]["A meta-markup language, used to create markup languages such as DocBook."]
                                        |        +-JSON.Pair[617, 672]["GlossSeeAlso", "GML", "XML"]
                                        |           +-JSON.String[617, 631]["GlossSeeAlso"]
                                        |           +-JSON.Value[633, 672]["GML", "XML"]
                                        |              +-JSON.Array[633, 672]["GML", "XML"]
                                        |                 +-JSON.Value[634, 639]["GML"]
                                        |                 |  +-JSON.String[634, 639]["GML"]
                                        |                 +-JSON.Value[641, 646]["XML"]
                                        |                    +-JSON.String[641, 646]["XML"]
                                        +-JSON.Pair[699, 740]["GlossSee", "markup"]
                                           +-JSON.String[699, 709]["GlossSee"]
                                           +-JSON.Value[711, 740]["markup"]
                                              +-JSON.String[711, 740]["markup"]
EOS");

    enum example4 =
    `{"web-app": {
    "servlet": [
        {
        "servlet-name": "cofaxCDS",
        "servlet-class": "org.cofax.cds.CDSServlet",
        "init-param": {
            "configGlossary:installationAt": "Philadelphia, PA",
            "configGlossary:adminEmail": "ksm@pobox.com",
            "configGlossary:poweredBy": "Cofax",
            "configGlossary:poweredByIcon": "/images/cofax.gif",
            "configGlossary:staticPath": "/content/static",
            "templateProcessorClass": "org.cofax.WysiwygTemplate",
            "templateLoaderClass": "org.cofax.FilesTemplateLoader",
            "templatePath": "templates",
            "templateOverridePath": "",
            "defaultListTemplate": "listTemplate.htm",
            "defaultFileTemplate": "articleTemplate.htm",
            "useJSP": false,
            "jspListTemplate": "listTemplate.jsp",
            "jspFileTemplate": "articleTemplate.jsp",
            "cachePackageTagsTrack": 200,
            "cachePackageTagsStore": 200,
            "cachePackageTagsRefresh": 60,
            "cacheTemplatesTrack": 100,
            "cacheTemplatesStore": 50,
            "cacheTemplatesRefresh": 15,
            "cachePagesTrack": 200,
            "cachePagesStore": 100,
            "cachePagesRefresh": 10,
            "cachePagesDirtyRead": 10,
            "searchEngineListTemplate": "forSearchEnginesList.htm",
            "searchEngineFileTemplate": "forSearchEngines.htm",
            "searchEngineRobotsDb": "WEB-INF/robots.db",
            "useDataStore": true,
            "dataStoreClass": "org.cofax.SqlDataStore",
            "redirectionClass": "org.cofax.SqlRedirection",
            "dataStoreName": "cofax",
            "dataStoreDriver": "com.microsoft.jdbc.sqlserver.SQLServerDriver",
            "dataStoreUrl": "jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon",
            "dataStoreUser": "sa",
            "dataStorePassword": "dataStoreTestQuery",
            "dataStoreTestQuery": "SET NOCOUNT ON;select test='test';",
            "dataStoreLogFile": "/usr/local/tomcat/logs/datastore.log",
            "dataStoreInitConns": 10,
            "dataStoreMaxConns": 100,
            "dataStoreConnUsageLimit": 100,
            "dataStoreLogLevel": "debug",
            "maxUrlLength": 500}},
        {
        "servlet-name": "cofaxEmail",
        "servlet-class": "org.cofax.cds.EmailServlet",
        "init-param": {
        "mailHost": "mail1",
        "mailHostOverride": "mail2"}},
        {
        "servlet-name": "cofaxAdmin",
        "servlet-class": "org.cofax.cds.AdminServlet"},

        {
        "servlet-name": "fileServlet",
        "servlet-class": "org.cofax.cds.FileServlet"},
        {
        "servlet-name": "cofaxTools",
        "servlet-class": "org.cofax.cms.CofaxToolsServlet",
        "init-param": {
            "templatePath": "toolstemplates/",
            "log": 1,
            "logLocation": "/usr/local/tomcat/logs/CofaxTools.log",
            "logMaxSize": "",
            "dataLog": 1,
            "dataLogLocation": "/usr/local/tomcat/logs/dataLog.log",
            "dataLogMaxSize": "",
            "removePageCache": "/content/admin/remove?cache=pages&id=",
            "removeTemplateCache": "/content/admin/remove?cache=templates&id=",
            "fileTransferFolder": "/usr/local/tomcat/webapps/content/fileTransferFolder",
            "lookInContext": 1,
            "adminGroupID": 4,
            "betaServer": true}}],
    "servlet-mapping": {
        "cofaxCDS": "/",
        "cofaxEmail": "/cofaxutil/aemail/*",
        "cofaxAdmin": "/admin/*",
        "fileServlet": "/static/*",
        "cofaxTools": "/tools/*"},

    "taglib": {
        "taglib-uri": "cofax.tld",
        "taglib-location": "/WEB-INF/tlds/cofax.tld"}}}
    `;
    assert(JSON(example4).successful);
}
