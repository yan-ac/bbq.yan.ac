function postJsonData (url, data, cb) {
  var req = new XMLHttpRequest();
  req.open('POST', url);
  req.setRequestHeader('Content-Type', 'application/json');
  req.send(JSON.stringify(data));
  req.onreadystatechange = function () {
    if (req.readyState != 4)
      return;
    if (req.status != 200)
      cb(req.response);
    else
      cb(null, req.response);
  }
}
