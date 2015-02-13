var selectPicture = document.getElementById('picture-select');

var currentURL;
var currentAngle;

selectPicture.addEventListener('change', function (event) {
  if (event.target.files.length != 1)
    return;

  currentURL = URL.createObjectURL(event.target.files[0]);
  currentAngle = 0;
  drawImage();
});

function rotate (deg) {
  currentAngle += deg;
  if (currentAngle < 0)
    currentAngle += 360;
  if (currentAngle >= 360)
    currentAngle -= 360;

  drawImage();
}

function drawImage () {
  var image = new Image();
  image.onload = function () {
    if (currentAngle == 90 || currentAngle == 270)
      var frameWidth = 1600,
          frameHeight = 1200;
    else
      var frameWidth = 1200,
          frameHeight = 1600;

    var w = image.naturalWidth,
        h = image.naturalHeight;

    if (h > frameHeight)
      w /= h / frameHeight,
      h = frameHeight;
    if (h > frameWidth)
      h /= w / frameWidth,
      w = frameWidth;

    var context = document.getElementById("canvas").getContext('2d');
    var radian = currentAngle * (Math.PI / 180);
    context.clearRect(0, 0, 1200, 1600);

    context.translate(600, 800);
    context.rotate(radian);
    context.drawImage(image, -(w / 2), -(h / 2), w, h);
    context.rotate(-radian);
    context.translate(-600, -800);
  };
  image.src = currentURL;
}


// Copied from http://stackoverflow.com/questions/4998908/convert-data-uri-to-file-then-append-to-formdata
function dataURItoBlob (dataURI) {
    // convert base64/URLEncoded data component to raw binary data held in a string
    var byteString;
    if (dataURI.split(',')[0].indexOf('base64') >= 0)
        byteString = atob(dataURI.split(',')[1]);
    else
        byteString = unescape(dataURI.split(',')[1]);

    // separate out the mime component
    var mimeString = dataURI.split(',')[0].split(':')[1].split(';')[0];

    // write the bytes of the string to a typed array
    var ia = new Uint8Array(byteString.length);
    for (var i = 0; i < byteString.length; i++) {
        ia[i] = byteString.charCodeAt(i);
    }

    return new Blob([ia], {type:mimeString});
}

function setURLBeforeToggleUploadOverlay (button) {
  button.addEventListener('click', function () {
    window.uploadTargetURL = getAttr(button, "data-upload");
  });
}
var uploadOverlayToggles = toArray(document.getElementsByClassName('js-upload-overlay'));
uploadOverlayToggles.forEach(setURLBeforeToggleUploadOverlay);

function process () {
  var dataURL = document.getElementById('canvas').toDataURL('image/jpeg', 0.5);
  var blob = dataURItoBlob(dataURL);
  var fd = new FormData();
  fd.append('sheet', blob);
  var xhr = new XMLHttpRequest();
  xhr.open('POST', window.uploadTargetURL, true);
  xhr.onload = function () { alert('上传成功'); };
  xhr.send(fd);
}
