module pegged.examples.json;

import pegged.grammar;

/// JSON
enum JSONGrammar =
   `JSON:
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
    
    String <~ :DoubleQuote Char* :DoubleQuote
    Char   <~ BackSlash DoubleQuote 
            / BackSlash BackSlash 
            / BackSlash [bfnrt] 
            / BackSlash 'u' Hex Hex Hex Hex
            / (!DoubleQuote .)
    
    Number <~ '0'
            / [1-9] Digit* ('.' Digit*)?
    Digit  <- [0-9]
    Hex    <- [0-9A-Fa-f]`
;

mixin(grammar(JSONGrammar));

unittest
{
    enum example1 = `{"Hello":42, "World":"!"}`;
    
    auto p1 = JSON.parse(example1);
    assert(p1.success);
    
    assert(p1.capture == ["Hello"d, "42"d, "World"d, "!"d]);
    assert(p1.parseTree.name == "JSON.JSONObject");
    
    assert(p1.parseTree.children[0].ruleName == "Pair");
    assert(p1.parseTree.children[0].capture == ["Hello"d,"42"d]);
    
    assert(p1.parseTree.children[1].ruleName == "Pair");
    assert(p1.parseTree.children[1].capture == ["World"d,"!"d]);
    
    enum example2 = `
{
    "Number": 42, 
    "Decimal": 123.456,
    "String": "abc",
    "Escape": "\uAAAA\n\\Hello",
    "Empty" : {},
    "Array" : [0,1,2],
    "Array2": [0, [0,1,2], "abc"],
    "Obj"   : { "Member":0, "Member":[0,1,2] },
    "True"  : true,
    "False" : false,
    "Null"  : null
}`;
    assert(JSON.parse(example2).success);

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
    
    assert(JSON.parse(example3).success);

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

    assert(JSON.parse(example4).success);
}

