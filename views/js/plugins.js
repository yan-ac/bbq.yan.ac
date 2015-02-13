function toArray (arrayLike) {
  return Array.prototype.slice.call(arrayLike);
}

function getAttr (elem, attrName) {
  var attrs = elem.attributes;
  if (!attrs[attrName])
    return '';
  return attrs[attrName].value;
}

function addToggleOverlayHandler (elem) {
  var targetId = getAttr(elem, 'data-overlay');
  var target = document.getElementById(targetId);
  elem.addEventListener('click', function (event) {
    target.classList.add('overlay-toggled');
    event.preventDefault();
  });
}

var overlayToggles = toArray(document.getElementsByClassName('js-toggle-overlay'));
overlayToggles.forEach(addToggleOverlayHandler);

function addCloseOverlayHandler (elem) {
  var targetId = getAttr(elem, 'data-overlay');
  var target = document.getElementById(targetId);
  elem.addEventListener('click', function (event) {
    target.classList.remove('overlay-toggled');
    event.preventDefault();
  });
}

var overlayClosers = toArray(document.getElementsByClassName('js-close-overlay'));
overlayClosers.forEach(addCloseOverlayHandler);

function addValformHandler (elem) {
  var submit    = elem.children.namedItem('submit');
  var method    = getAttr(elem, 'method');
  var targetUrl = getAttr(elem, 'action');

  submit.addEventListener('click', function (event) {
    var formData = new FormData(elem);
    var xhr = new XMLHttpRequest();

    xhr.open(method, targetUrl);
    xhr.onload = getValformUploadedCallback(elem);
    xhr.send(formData);

    event.preventDefault();
  });
}

var valforms = toArray(document.getElementsByClassName('js-valform'));
valforms.forEach(addValformHandler);

function getValformUploadedCallback (form) {
  var messageBox = form.children.namedItem('message');

  function showOK (msgBox, msg) {
    msgBox.classList.remove('label-error');
    msgBox.classList.add('label-ok');
    msgBox.innerText = msg;
  }

  function showError (msgBox, msg) {
    msgBox.classList.remove('label-ok');
    msgBox.classList.add('label-error');
    msgBox.innerText = msg;
  }

  function redirect (url) {
    window.location.href = url;
  }

  return function (event) {
    var text = this.responseText;
    if (text.slice(0, 2) == 'OK' && messageBox != null)
      showOK(messageBox, text.slice(2));
    if (text.slice(0, 5) == 'ERROR' && messageBox != null)
      showError(messageBox, text.slice(5));
    if (text.slice(0, 8) == 'REDIRECT')
      redirect(text.slice(8));
  }
}

