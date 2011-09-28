YUI.add('mappingtable', function(Y) {

	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;

	var NODE_SOURCE_INFO = Y.one("#sourceinfo"),
		NODE_TARGET_INFO = Y.one("#targetinfo");

	function MappingTable(config) {
		MappingTable.superclass.constructor.apply(this, arguments);
	}
	MappingTable.NAME = "mappingtable";
	MappingTable.ATTRS = {
		srcNode: {
			value: null
		},
		rows: {
			value:100,
			validator:function(val) {
				return Lang.isNumber(val);
			}
		},
		alignment: {
			value: null
		},
		mapping: {
			value: null
		},
		datasource: {
			value: null
		}
	};

	Y.extend(MappingTable, Y.Base, {
		initializer: function(config) {
			var instance = this,
				content = this.get("srcNode");

			this.table = new Y.DataTable.Base({
				columnset:[{key:"source",
					       formatter:this.formatResource,
					       sortable:true
					      },
					      {key:"relation",
					       formatter:this.formatRelation,
					       sortable:true
					      },
					      {key:"target",
					       formatter:this.formatResource,
					       sortable:true
					      }],
				plugins: [ Y.Plugin.DataTableSort ]
			})
			.render(content.appendChild(Node.create(
				'<div class="table"></div>'
			)));

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

			// get new data if mapping is changed
			this.after('mappingChange', function() {this.loadData()}, this);
			this.table.delegate('click', this._onRowSelect, '.yui3-datatable-data tr', this);
			this.loadData();
		},

		loadData : function(conf, recordsOnly) {
			var mapping = this.get("mapping"),
				datasource = this.get("datasource"),
				alignment = this.get("alignment"),
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
					table.set("recordset", records);
				}
			};

			if(mapping) {
				conf = conf ? conf : {};
				conf.url = mapping;
				conf.alignment=alignment;
				datasource.sendRequest({
					request:'?'+Y.QueryString.stringify(conf),
					callback:callback
				})
			} else {
				paginator.setTotalRecords(0, true);
				table.set("recordset", []);
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
			var row = e.currentTarget,
				records = this.table.get("recordset"),
                                current = records.getRecord( e.currentTarget.get("id"));
			var data = {
					row:row,
					sourceConcept: current.getValue("source"),
					targetConcept: current.getValue("target"),
					relation:current.getValue("relation")
				};
			//if(!add) {
				Y.all(".yui3-datatable tr").removeClass("yui3-datatable-selected");
			//}
			row.addClass("yui3-datatable-selected");
			this.fire("rowSelect", data)
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
			      Y.log(end.getXY());
			      return row; // fix me
			    }
			  },
		nextRecord : function(row) {
			       var next = this.nextRow(row),
			         records = this.table.get("recordset");
			         return records.getRecord(next.get("id"));
			     },
		prevRecord : function(row) {
			       var prev = this.prevRow(row),
			         records = this.table.get("recordset");
			         return records.getRecord(prev.get("id"));
			     },
	});

	Y.MappingTable = MappingTable;

}, '0.0.1', { requires: ['node,event','gallery-paginator','datatable','datatable-sort']});
