# OmniXML
Simple way to use XML in Delphi

OmniXML is a XML parser written in Delphi.

* Full support for Document Object Model (DOM) Level 1 specification.
* Supports Extensible Markup Language (XML) 1.0 (Second Edition) specification.
* Has built-in support for different code pages (main 8-bit code pages, UTF-8, UTF-16).
* Is compatible with MS XML parser.
* Fast parsing even large and highly structured documents.
* Includes helper functions to ease processing XML documents.

OmniXML is developed by Miha Remec, Primož Gabrijelčič and contributors. Copyright © 2002-2016 by Miha Remec and Primož Gabrijelčič.

# This fork
This fork adds

* De/serialize class properties as xml element or xml attributes
* Serialize your class soon after xml declaration without first 'data' tag
* Custom names for xml elements and xml attributes
* De/serialize ObjectList as multiple tags (when maxOccurs>1)
* De/serialize interfaces
* Serialize [Spring4D](https://bitbucket.org/sglienke/spring4d/src/master/) IList<Interface> (cannot be deserialized)

This fork removes

* De/serialize *ALL* class properties as xml element or *ALL* class properties as xml attribute 
* No more serialize 'data' xml element from every serialized class
