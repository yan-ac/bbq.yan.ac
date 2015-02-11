function toArray (arrayLike) {
  return Array.prototype.slice.call(arrayLike);
}

function addToggleOverlayHandler (elem) {
  var targetId = elem.attributes['data-overlay'].value;
  var target = document.getElementById(targetId);
  elem.addEventListener('click', function (event) {
    target.classList.add('overlay-toggled');
    event.preventDefault();
  });
}

var overlayToggles = toArray(document.getElementsByClassName('js-toggle-overlay'));
overlayToggles.forEach(addToggleOverlayHandler);

function addCloseOverlayHandler (elem) {
  var targetId = elem.attributes['data-overlay'].value;
  var target = document.getElementById(targetId);
  elem.addEventListener('click', function (event) {
    target.classList.remove('overlay-toggled');
    event.preventDefault();
  });
}

var overlayClosers = toArray(document.getElementsByClassName('js-close-overlay'));
overlayClosers.forEach(addCloseOverlayHandler);
