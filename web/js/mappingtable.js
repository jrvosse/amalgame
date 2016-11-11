YUI.add('mappingtable', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		SOURCE_COLUMN = 0,
		TARGET_COLUMN = 1,
		Widget = Y.Widget;

	function MappingTable(config) {
		MappingTable.superclass.constructor.apply(this, arguments);
	}
	MappingTable.NAME = "mappingtable";
	MappingTable.ATTRS = {
		srcNode: { value: null },
		strategy: { value: null },
		focus: { value: null },
		datasource: { value: null },
		showRelation: { value: false },
		vocs: { value: { source: null, target: null }},
		rows: { value:100,
			validator:function(val) {
				return Lang.isNumber(val);
			}},
		loading: { value:false,
			validator:function(val) {
				return Lang.isBoolean(val);
			}}
	};

	Y.extend(MappingTable, Y.Base, {
		initializer: function(config) {
			var instance = this,
				content = this.get("srcNode");

			this._tableNode = content.appendChild(Node.create(
				'<div class="table"></div>'
			));
			this._loadingNode = content.appendChild(Node.create(
				'<div class="loading"></div>'
			));
			var s_column = { key:"source", 
					 formatter:this.formatResource,
				 	 allowHTML: true,
					 sortable:true };
			var t_column = { key:"target", 
				         formatter:this.formatResource,
				 	 allowHTML: true,
					 sortable:true };
			var r_column = { key:"relation", label: 'manually assigned relation',
					 formatter:this.formatRelation,
				 	 allowHTML: true,
					 sortable:true };
			var columns = [];
			SOURCE_COLUMN = 0
			if (this.get('showRelation')) {
				columns = [s_column, r_column, t_column];
				TARGET_COLUMN = 2;
			} else {
				columns = [s_column, t_column];
			}

			this.table = new Y.DataTable({columns:columns}).render(this._tableNode);

			this.paginator = new Y.Paginator({
				rowsPerPage:this.get("rows"),
				template: '{FirstPageLink} {PreviousPageLink} {PageLinks} {NextPageLink} {LastPageLink}',
				firstPageLinkLabel:'|&lt;',
				previousPageLinkLabel: '&lt;',
				nextPageLinkLabel: '&gt;',
				lastPageLinkLabel: '&gt;|'
			})
			.render(content.appendChild(Node.create(
				'<div class="paginator"></div>'
			)));
			this.paginator.on("changeRequest", function(state) {
				this.setPage(state.page, true);
				instance.loadData({offset:state.recordOffset}, true);
			});
			this.on('loadingChange', this._onLoadingChange, this);
			
			// get new data if mapping is changed
			this.after('focusChange', function() {this.loadData()}, this);
			this.table.delegate('click', this._onRowSelect, '.yui3-datatable-data tr', this);
			this.loadData();
		},

		loadData : function(conf, recordsOnly) {
			var oSelf = this,
				s = this.get("focus"),
				datasource = this.get("datasource"),
				strategy = this.get("strategy"),
				table = this.table,
				paginator = this.paginator;

			var callback =	{
				success: function(o) {
					var records = o.response.results,
						total = o.response.meta.totalNumberOfResults;
					if(!recordsOnly) {
						paginator.setPage(1, true);
						paginator.setTotalRecords(total, true);
					}
					var vocs = o.response.meta.stats.vocs;
					if (vocs) {
						var sL = vocs.source.label + ' (source terms from: ' + s.abbrev + '.' + s.label + ')';
						var tL = vocs.target.label + ' (target terms from: ' + s.abbrev + '.' + s.label + ')';
						table.head.columns[0][SOURCE_COLUMN].label = sL;
						table.head.columns[0][TARGET_COLUMN].label = tL;
						table.set("data", records);
						table.syncUI();
						oSelf.set("vocs", vocs);
					}
					oSelf.set("loading", false);
				}
			};

			if(s) { // selected mapping s
				conf = conf ? conf : {};
				conf.url = s.uri;
				conf.strategy=strategy;
				conf.limit=this.get('rows');
				this.set("loading", true);
				datasource.sendRequest({
					request:'?'+Y.QueryString.stringify(conf),
					callback:callback
				})
			} else {
				paginator.setTotalRecords(0, true);
				table.set("data", []);
			}
		},

		formatResource : function(o) {
			var label = o.value ? o.value.label : "";
			return "<div class=resource>"+label+"</div>";
		},
		formatRelation : function(o) {
			var label = o.value ? o.value.label : "";
			label = label?label:"";
			return "<div class=relation>"+label+"</div>";
		},

		_onRowSelect : function(e) {
			var row = e.currentTarget;
				current = this.table.getRecord(e.target);
				source = current.get("source");
				target = current.get("target");
				
			var data = {
					row:row,
					focus:current,
					sourceConcept: source,
					targetConcept: target,
					vocs: this.get("vocs"),
					relation:current.get("relation")
				};
			Y.all(".yui3-datatable tr").removeClass("yui3-datatable-selected");
			row.addClass("yui3-datatable-selected");
			// Y.log("selected correspondence in mappingtable: "+source.uri+ " - " +target.uri);
			// Y.log(current);
			this.fire("rowSelect", data);
		},

		nextRow : function(row) {
			    var rows = row.get("parentNode").all("tr"),
			        i = rows.indexOf(row);

			    if (++i < rows.size()) {
			      return rows.item(i);
			    } else {
			      var begin = rows.item(0);
			      this.fire("wrapAround", begin.getXY());
			      return row; // fix me
			    }
			  },
		prevRow : function(row) {
			    var rows = row.get("parentNode").all("tr"),
			        i = rows.indexOf(row);
			    if (--i >= 0) {
			        return rows.item(i);
			    } else {
			      var end = rows.item(rows.size() - 1);
			      this.fire("wrapAround", null);
			      return row; // fix me
			    }
			  },
		nextRecord : function(row) {
			       var next = this.nextRow(row);
			       return this.table.getRecord(next.get("id"));
			     },
		prevRecord : function(row) {
			       var prev = this.prevRow(row);
			       return this.table.getRecord(prev.get("id"));
			     },
			
		addRow    : function(value)         { this.table.addRow(value); },
		removeRow : function(record)        { this.table.removeRow(record); },
		modifyRow : function(record, value) { this.table.modifyRow(record, value); },

		_onLoadingChange : function (o) {
			if(o.newVal) {
				this._tableNode.addClass("hidden");
				this._loadingNode.removeClass("hidden");
			} else {
				this._loadingNode.addClass("hidden");
				this._tableNode.removeClass("hidden");
			}
		}
	});

	Y.MappingTable = MappingTable;

}, '0.0.1', { requires: ['node,event','gallery-paginator','datatable','datatable-sort']});
