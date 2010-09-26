/*  This file is part of ClioPatria.

    Author:	Jacco van Ossenbruggen, Michiel Hildebrand & Anna Tordai
    HTTP:	http://e-culture.multimedian.nl/software/ClioPatria.shtml
    GITWEB:	http://eculture.cs.vu.nl/git/ClioPatria.git
    GIT:	git://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    GIT:	http://eculture.cs.vu.nl/home/git/eculture/ClioPatria.git
    Copyright:  2005-2009, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/


YAHOO.namespace("mazzle");

/* JavaScript routines used in the ClioPatria tool
 * to manually check candidate mappings, usually on /api/evaluator/
 */

var mappingsLeftForConcept = 0;

/**
 * map check
 *
 * @class MapCheck
 * @constructor
 * @param elContainer {HTMLElement} DOM element reference of an existing DIV.
 * @param elContainer {String} String ID of an existing DIV.
 * @param oConfigs {Object} Object literal with configuration params.
 */
YAHOO.mazzle.MapCheck = function(elContainer, oConfigs) {
  if(!YAHOO.util.Dom.inDocument(elContainer)) return;
  else
    this._elContainer = YAHOO.util.Dom.get(elContainer);

  // Set any config params passed in to override defaults
  if(oConfigs && (oConfigs.constructor == Object)) {
    for(var sConfig in oConfigs) {
      if(sConfig) {
	this[sConfig] = oConfigs[sConfig];
      }
    }
  }
  // init the components
  this._initTree();
  this._initContainer();
  this._initMap('head');
};

YAHOO.mazzle.MapCheck.prototype._initMap = function(headornext) {

  function successhandler(o) {
    var response = YAHOO.lang.JSON.parse(o.responseText);
    if(!response.mappings) {
      window.alert('Thanks a lot! After your press OK you will be redirected to the page showing the saved RDF graph containing your results');
      window.location.href=response.redirect;
      return;
    }

    this._elCounter.innerHTML=" (still " + response.nr_to_go + " concepts to go)";


    this.mappings  = response.mappings;
    this.display = response.display;
    this.uri1 = this.mappings[0].subject.value
    this.uri2 = this.mappings[0].object.value

    var elResource1 = this._initResource({id:1, uri:this.uri1});
    elResource1.setAttribute('style', 'width: 49%');
    var elResource3 = this._initResource({id:3, uri:this.uri2});
    elResource3.setAttribute('style', 'width: 49%');

    var rdfsubprop = 'http://www.w3.org/2000/01/rdf-schema#subPropertyOf';
    var expand = (this.mappings.length >1)?false:true;
    mappingsLeftForConcept=this.mappings.length;

    for(i in this.mappings) {
      if (i==0) {
	var uri1 = this.mappings[i].subject.value;
	this._initForm({nr:i, id:1, uri:uri1, container:elResource1, expand:expand});
      };
      var uri2 = this.mappings[i].predicate.value;
      var ns = this.display[uri2].ns[0].value;
      var pred = uri2.replace(ns, "");
      var uri3 = this.mappings[i].object.value;
      this._initForm({nr:i, id:3, uri:uri3, pred:pred, container:elResource3, expand:expand, total:this.mappings.length});
    };
  };
  function failurehandler() { };

  var link = serverPrefix()+'/api/evaluator/get?method='+headornext+
	  '&target='+encodeURIComponent(this.targetgraph)+
	  '&graph='+encodeURIComponent(this.graph);
  var oCallback = { success:successhandler,
		    failure:this.failurehandler,
		    scope: this
  };
  var request = YAHOO.util.Connect.asyncRequest('GET', link, oCallback);

};

/////////////////////////////////////////////////////////////////////////////
//
// Public methods
//
/////////////////////////////////////////////////////////////////////////////

YAHOO.mazzle.MapCheck.prototype.loadData = function(oParams) {
	if(oParams) {
		this.params = oParams;
	}
	var sQueryString = this._initQueryString();

	var oCallback = {
    		success:this._processResults,
    		failure:this._errorHandler,
    		scope: this
	};
	this._oDataSource.sendRequest(sQueryString, oCallback);
	this.show();
	this.center();
};

/////////////////////////////////////////////////////////////////////////////
//
// Private methods
//
/////////////////////////////////////////////////////////////////////////////

/**
 * Initializes the container and add the content
 *
 * @method _initContainer
 * @private
 */
YAHOO.mazzle.MapCheck.prototype._initContainer = function() {

	// Create HTML elements
	var elPanelHeader = document.createElement("div");
	elPanelHeader.className = "hd";
        elPanelHeader.id="panelheader1";
	this._elHeader = this._elContainer.appendChild(elPanelHeader);

	var elBody = document.createElement("div");
	elBody.className = "yui-gb";
	this._elBody = this._elContainer.appendChild(elBody);

	var elPanelFooter = document.createElement("div");
	elPanelFooter.className = "ft";
	this._elFooter = this._elContainer.insertBefore(elPanelFooter,elBody);

// 	this._oNextButton = new YAHOO.widget.Button({
// 	       id:"nextButton",
// 	       type: "Button",
//                label: "Skip/next",
//                container: elPanelFooter
// 	});
//         this._oNextButton.addListener("click", this.goto_next , {oSelf:this});

// 	this._oSaveButton = new YAHOO.widget.Button({
// 	       id:"saveButton",
// 	       type: "Button",
//                label: "Save intermediate results to file",
//                container: elPanelFooter
// 	});
//         this._oSaveButton.addListener("click", this.save_results , {oSelf:this});


        var counter = document.createElement('span');
        counter.className="counter mappings_to_go";
        this._elCounter = this._elFooter.appendChild(counter);

	this.elTarget = [];
	// return this._initSourceMenu(elPanelFooter);
	return;
};

YAHOO.mazzle.MapCheck.prototype._judgeCallback = function(ev,oParams) {
  oSelf = oParams.oSelf;
  index = oParams.index;
  mapping = oSelf.mappings[index];
  choice = oParams.choice;
  comment = oParams.comment.value;

  console.log(comment);
  function successhandler() { oSelf.delete_mapping(ev,oParams); };
  function failurehandler() { };
  var callback = { success:successhandler,
		   failure:failurehandler };
  sLink=serverPrefix() + "/api/evaluator/judge" +
  "?" + queryString('judgement', choice) +
  "&" + queryString('subject',     mapping.subject.value) +
  "&" + queryString('predicate',    mapping.predicate.value) +
  "&" + queryString('object',     mapping.object.value) +
  "&" + queryString('target',  oSelf.targetgraph) +
  "&" + queryString('comment',  comment) ;
  var request = YAHOO.util.Connect.asyncRequest('GET', sLink, callback);
};


YAHOO.mazzle.MapCheck.prototype.goto_next = function(event, oParams) {
  var oSelf = oParams.oSelf;
  var elBody = oSelf._elBody;
  while(elBody.firstChild) { elBody.removeChild(elBody.firstChild) };
  oSelf._initMap('next');
};

YAHOO.mazzle.MapCheck.prototype.save_results = function(event , oParams) {
  var oSelf = oParams.oSelf;
  sLink=serverPrefix()+'/api/evaluator/save?' +
  queryString('file', oSelf.graph);
  function successhandler(o) {
	  response = YAHOO.lang.JSON.parse(o.responseText);
	  window.alert('Save in server-side dir ' + response.dir +' succeeded');
  };
  function failurehandler() { window.alert('Save failed')};
  var callback = { success:successhandler,
		   failure:failurehandler,
  };
  var request = YAHOO.util.Connect.asyncRequest('GET', sLink, callback);
}

YAHOO.mazzle.MapCheck.prototype.delete_mapping = function(event, oParams) {
  var index = oParams.index;
  var oSelf = oParams.oSelf;
  var target = oSelf.elTarget[index];
  var parent = target.parentNode;
  var grandparent = parent.parentNode;
  mappingsLeftForConcept = mappingsLeftForConcept - 1;
  parent.removeChild(target);
  var elChoice=document.createElement('div');
  elChoice.innerHTML=oParams.choice;
  parent.appendChild(elChoice);
  if (!mappingsLeftForConcept) // we're done with all mappings for this concept, get next
    oSelf.goto_next(event, oParams);
};


YAHOO.mazzle.MapCheck.prototype._initResource = function(oParms) {
	var id = oParms.id;
	var sURI = oParms.uri;
	var prop = oParms.prop;

	var elResource = document.createElement("div");
	elResource.className =(id==1)?"resource yui-u first":"resource yui-u";
	elResource.id = "resourcediv"+id;
	this._elBody.appendChild(elResource);

	return elResource;
};

YAHOO.mazzle.MapCheck.prototype._initForm = function(oParms) {
  var nr = oParms.nr;
  var id = oParms.id;
  var sURI = oParms.uri;
  var pred = oParms.pred;
  var elContainer = oParms.container;
  var expand = oParms.expand;
  var hierarchy = oParms.hierarchy;
  var total = oParms.total;

  var elForm = document.createElement("form");
  elForm.id="searchField"+id+'_'+nr;  elContainer.appendChild(elForm);

  var yuiWrapper = document.createElement("div");
  yuiWrapper.className="yui-ac-input-wrapper"; elForm.appendChild(yuiWrapper);

  var elURI = document.createElement("a");
  elURI.id = 'conceptURI'+id+'_'+nr;
  elURI.href = serverPrefix() + '/../browse/list_resource?r=' + encodeURIComponent(sURI) ;
  elURI.innerHTML = sURI;
  yuiWrapper.appendChild(elURI);

  var dis1 = this.display[sURI];
  if (dis1) {
  	var oLabels = {};
  	for (p in dis1) {
    	oLabels[p] = dis1[p][0].value;
  	};
  	var examples = oLabels['examples'];
  	var uri1Label = oLabels['label'];
  	var elLabels = document.createElement('div');
  	elLabels.className = 'acItem';
  	elLabels.innerHTML = formatItem(uri1Label, dis1, true);
  	elLabels.setAttribute('style', 'border: 1pt solid; padding: 0.1em 1.3em;');
  	elForm.appendChild(elLabels);

  	if (examples && examples[0] && examples[0].value != []) {
		var elExamples = document.createElement('div');
		var elExampleT = document.createElement('div');
		elExampleT.innerHTML='Example usage:';
		elExamples.appendChild(elExampleT);
		elForm.appendChild(elExamples);
		for (ex in examples) {
			var elImg = document.createElement('img');
			elImg.setAttribute('src',examples[ex]);
			elImg.setAttribute('style', 'padding: 2px;');
			elExamples.appendChild(elImg);
		}
  	}
  }
  var elWrapper = document.createElement("div");
  this.elTarget[i] = elWrapper;
  elWrapper.setAttribute("style", "border-bottom: 1pt dashed; margin-bottom: 5pt;");
  elForm.appendChild(elWrapper);

 if (id==3) {
    var elButtons = document.createElement("div");
    elWrapper.appendChild(elButtons);
    elButtons.setAttribute('id', "target"+i);
    var elCounter = document.createElement('div');
    var i_plus_one = parseInt(i)+1;
    elCounter.innerHTML = "target concept " + i_plus_one + ' (was: ' + pred + '?) out of ' + total + ' mappings: ';
    elButtons.appendChild(elCounter);
    var elAppReject  = document.createElement("div");
    var elSkos  = document.createElement("div");
    var elUnsure  = document.createElement("div");
    var elComment = document.createElement("input");
    var elCommentLabel = document.createElement("span");
    elCommentLabel.innerHTML = "comment (optional): ";
    elCommentLabel.setAttribute("class", "commentLabel");
    elComment.setAttribute("type", "text");
    elComment.setAttribute("name", "comment");
    elComment.setAttribute("size", "50");

    elButtons.appendChild(elAppReject);
    elButtons.appendChild(elSkos);
    elButtons.appendChild(elUnsure);
    elButtons.appendChild(elCommentLabel);
    elButtons.appendChild(elComment);


    ButtonType="push";

    this._oExactButton = new YAHOO.widget.Button({
      id:"exactButton"+i,
	  type: ButtonType,
	  label: "Exact match",
	  container: elAppReject,
	  });
    this._oExactButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'skos:exactMatch', comment:elComment});

    this._oCloseButton = new YAHOO.widget.Button({
      id:"closeButton"+i,
	  type: ButtonType,
	  label: "close match",
	  container: elAppReject,
	  });
    this._oCloseButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'skos:closeMatch'});

    this._oBroaderButton = new YAHOO.widget.Button({
      id:"broaderButton"+i,
	  type: ButtonType,
	  label: "Broader",
	  container: elSkos,
	  });
    this._oBroaderButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'skos:broadMatch'});

    this._oNarrowerButton = new YAHOO.widget.Button({
      id:"narrowerButton"+i,
	  type: ButtonType,
	  label: "narrower",
	  container: elSkos,
	  });
    this._oNarrowerButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'skos:narrowMatch'});

    this._oRelatedButton = new YAHOO.widget.Button({
      id:"RelatedButton"+i,
	  type: ButtonType,
	  label: "related match",
	  container: elSkos,
	  });
    this._oRelatedButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'skos:relatedMatch'});

    this._oUnsureButton = new YAHOO.widget.Button({
      id:"unsureButton"+i,
	  type: ButtonType,
	  label: "? I'm not sure ?",
	  container: elUnsure,
	  });
    this._oUnsureButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'evaluator:unsure'});

    this._oUnrelatedButton = new YAHOO.widget.Button({
      id:"unrelatedButton"+i,
	  type: ButtonType,
	  label: "not related",
	  container: elAppReject,
	  });
    this._oUnrelatedButton.addListener("click", this._judgeCallback, {oSelf:this, index:i, choice:'evaluator:unrelated'});


  };

  var elResourceTree = document.createElement("div");
  elResourceTree.className = "resource-tree";
  elWrapper.appendChild(elResourceTree);
  this._loadTree(elResourceTree, sURI, hierarchy, expand);
};

/**
 * Initialize tree that is optionally shown in the secondairy field
 *
 * @method _initTree
 * @private
 */
YAHOO.mazzle.MapCheck.prototype._initTree = function() {

    // datasource
    var oServer  = serverPrefix()+"/api/evaluator/concept?";
    var	sQueryString = "&type=tree";

    var oResponseSchema = {
		resultsList : "result"
	};

    this._oTreeDataSource = new YAHOO.util.DataSource(oServer);
    this._oTreeDataSource.responseType = YAHOO.util.DataSource.TYPE_JSON;
    this._oTreeDataSource.scriptQueryAppend = sQueryString;
    this._oTreeDataSource.responseSchema = oResponseSchema;
    this._oTreeDataSource.maxCacheEntries = 20;
};


YAHOO.mazzle.MapCheck.prototype._loadTree = function(id, sURI, prop, expandChildren) {
	var oSelf = this;

	function successHandler(oRequest,oResponse){
	    var tree = oResponse.results[0];
		// init the tree
    	var oTree = new YAHOO.widget.TreeView(id);
    	var oRoot = oTree.getRoot();
    	oSelf.initTreeNode(oRoot, tree, prop, expandChildren);

    	// draw the tree
    	oTree.draw();

    	// click handler
    	oTree.subscribe("labelClick", function(oNode) {
            //oSelf._submit(oNode.data.uri, oNode.data.label);
        });
	}
	function failureHandler(o){
	    // just to bad
	}

	var oCallback = {
		success:successHandler,
		failure:failureHandler
	};

    // create request
	var request = queryString("r", sURI);
	if(this.display[sURI]&&this.display[sURI].ns) {
		var ns = this.display[sURI].ns[0].value;
		request += "&"+queryString("ns", ns);
	}

	if(prop&&prop!==true) {
	    request += "&"+queryString("rel", prop);
	}
	request += this._oTreeDataSource.scriptQueryAppend;

	this._oTreeDataSource.sendRequest(request, oCallback);
};


YAHOO.mazzle.MapCheck.prototype.initTreeNode = function(oRoot, oNode, sProp, expandChildren) {
    var oSelf = this;

   // function to load data for node
    var loadDataForNode = function(node, onCompleteCallback) {
        var sURI = node.data.uri;
    	// server request
    	var link = serverPrefix()+"/api/concepts";
    	link += "?parent="+encodeURIComponent(sURI);
    	if(sProp) {
	        link += "&"+queryString("rel", sProp);
	    }
    	link += "&type=child";

    	function successHandler(o){
    		var response = YAHOO.lang.JSON.parse(o.responseText);
    		var children = response.results
    		for(var i=0;i<children.length;i++) {
    		    var uri = children[i].id;
    			var oChild = {
    			    uri: uri,
    			    label: children[i].label,
    			    children: children[i].hasNext
    			};
    			oSelf.initTreeNode(node, oChild, sProp, expandChildren);
    		}
    		onCompleteCallback();
    	}
    	function failureHandler(o){
    	    onCompleteCallback();
    	}

    	var callback = {
    		success:successHandler,
    		failure:failureHandler
    	};
    	var request = YAHOO.util.Connect.asyncRequest('GET', link, callback);
    };

    var children = oNode.children?oNode.children:[];
    var label = oNode.label;
    if(label.length>50) {
        label = label.substr(0,45)+'...';
    }
    var labelClass = oNode.hit&&"hit"||"node";
    label = '<span title="'+oNode.label+'" class="'+labelClass+'">'+label+'</span>';

    var oData = {"label":label,"uri":oNode.uri};

    var tmpNode;
    if(children.constructor==Array&&children.length>0) {
	if (!expandChildren && oNode.hit) {
          tmpNode = new YAHOO.widget.TextNode(oData,oRoot);
          tmpNode.setDynamicLoad(loadDataForNode, 1);
	} else if (!expandChildren && children[0].hit) {
	  oData.expanded = true;
          tmpNode = new YAHOO.widget.TextNode(oData,oRoot);
          this.initTreeNodes(tmpNode, [oNode.children[0]], expandChildren);
	} else {
	  oData.expanded = true;
          tmpNode = new YAHOO.widget.TextNode(oData,oRoot);
          this.initTreeNodes(tmpNode, oNode.children, expandChildren);
	}
    }
    else if(children===true) {
        tmpNode = new YAHOO.widget.TextNode(oData,oRoot);
        tmpNode.setDynamicLoad(loadDataForNode, 1);
    }
    else {
        tmpNode = new YAHOO.widget.TextNode(oData,oRoot);
    }
};

YAHOO.mazzle.MapCheck.prototype.initTreeNodes = function(oRoot, aNodes, expandChildren) {
	for (var i=0;i<aNodes.length&&i<100;i++) {
        this.initTreeNode(oRoot, aNodes[i], false, expandChildren);
	}
};


// Array Remove - By John Resig (MIT Licensed)
Array.remove = function(array, from, to) {
  var rest = array.slice((to || from) + 1 || array.length);
  array.length = from < 0 ? array.length + from : from;
  return array.push.apply(array, rest);
};


/**
 * format HTML element with the labels specified in oShow
 *
 * @method formatItem
 * @param oShow {String[]} Array with labels to be shown
 * @return {String} HTML markup of a single formatted result item.
 * @public
 */
var formatItem = function(sMatch, oInfo, bHTML) {
        var sPreLabel, sAltLabel, sExtLabel, sEndLabel;
        var sSubLabel = '';

        if(oInfo) {
                if (oInfo.altlabel) sAltLabel = oInfo.altlabel; else sAltLabel = {};
                if (oInfo.prelabel) sPreLabel = oInfo.prelabel[0].value; else sPreLabel = '';
                if (oInfo.deflabel) sDefLabel = oInfo.deflabel[0].value; else sDefLabel = '';
                if (oInfo.scopelabel) sScopeLabel = oInfo.scopelabel[0].value; else sScopeLabel = '';
                if (oInfo.sublabel) sSubLabel = oInfo.sublabel[0].value; else sSubLabel = '';
                if (oInfo.preflabel) {
			sMatch= oInfo.preflabel[0].value;
			separator = ', ';
		}
		else {
			sMatch ='';
			separator ='';
		}
        }

        if(bHTML) {
                sLabel =  sPreLabel ? '<span class="acPreLabel">['+sPreLabel+']&nbsp;</span>' : '';
                sLabel += sMatch ? '<span class="acMatchLabel">'+sMatch+'</span>' : '';
		sLabel += separator;
		for (var alt in oInfo.altlabel) {
			sLabel += alt==0 ? '' : ', ';
			sLabel += oInfo.altlabel[alt].value;
		}
                sLabel += sDefLabel ? '<div class="acDefLabel">'+sDefLabel+'</div>' : '';

                sHTML =   '';
                sHTML +=  '<div class="acLabel">'+sLabel+'</div>';
                sHTML +=  '<div class="acSubLabel">'+sSubLabel+'</div>';
		sHTML +=  sScopeLabel ? '<div class="acScopeLabel"><span class=scopelabel>scope:</span> '+sScopeLabel+'</div>' : '';
                return sHTML;
        }
        else {
                sLabel =  sPreLabel&&'['+sPreLabel+'] '||'';
                sLabel += sMatch&&sMatch||'';
                sLabel += sAltLabel&&' ('+sAltLabel+')'||'';
                sLabel += sExtLabel&&', '+sExtLabel||'';
                sLabel += sEndLabel&&' '+sEndLabel||'';
                return sLabel;
        }

};

