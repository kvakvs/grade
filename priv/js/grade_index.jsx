var AppsDropdown = React.createClass({
  getDefaultProps: function() {
    var response = $.ajax({
      dataType: "json", type: "GET",
      url: '/grade/grade_web_esi:list_apps', async: false}).responseJSON;
    // Response format:
    // {nodes: {local_erlang_node: [app1, app2...], other_node: [app3, app4, ...]}}
    return {nodes: response.nodes};
  },

  // Create dropbox with applications per node
  render: function() {
    var items = [];
    var nodes = this.props.nodes;
    for(var n in nodes) {
      if (nodes.hasOwnProperty(n)) {
        items.push(<option disabled className="dropTitle">{n}</option>);

        var apps = this.props.nodes[n];
        for(var app in apps) {
          if (apps.hasOwnProperty(app)) {
            items.push(<option value={apps[app] + '@' + n}>{apps[app]}</option>);
          }
        }
      }
    }
    return (
      <select id="apps">{items}</select>
    );
  }
});

React.render(
  <AppsDropdown />,
  document.getElementById('appsList')
);
