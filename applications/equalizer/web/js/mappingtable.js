YUI.add('mappingtable', function(Y) {
	
	var Lang = Y.Lang,
		Node = Y.Node,
		Widget = Y.Widget;
	
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
		selected: {
			value: null
		},
		datasource: {
			value: null
		}
	};
	
	Y.extend(MappingTable, Y.Base, {
		initializer: function(config) {
			var instance = this,
				content = this.get("srcNode"),
				fetchFn = this.get("fetch");

			this.table = new Y.DataTable.Base({
				columnset:[{key:"source",
					       formatter:this.formatResource,
					       sortable:true
					      },
					      {key:"relation",
					       formatter:this.formatResource,
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
			/*	
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
				instance.updateRecords({offset:state.recordOffset});
			});*/
		},
		
		handleResponse : function(o) {
			var table = this.table,
				paginator = this.paginator,
				records = o.response.results,
				total = o.response.meta.totalNumberOfResults;
			
			//paginator.setPage(1, true);
			//paginator.setTotalRecords(total, true);
			table.set("recordset", records);
		},
		
		updateRecords : function(conf) {
			var selected = this.get("selected"),
				DS = this.get("datasource"),
				table = this.table;
			
			if(selected) {
				conf = conf ? conf : {};
				conf.url = selected;
				DS.sendRequest({
					request:'?'+Y.QueryString.stringify(conf),
					callback: {success: function(o) {
						table.set("recordset", o.response.results);
					}}
				})
			}	
		},
		
		formatResource : function(o) {
			var label = o.value ? o.value.label : "";
     		return "<div class=resource>"+label+"</div>";
		}
		
	});
	
	Y.MappingTable = MappingTable;
	
}, '0.0.1', { requires: ['node,event','gallery-paginator','datatable','datatable-sort']});