
function h$bytestring_is_valid_utf8(bs) {
  return true;
}

function setInner(html) {
  document.body.innerHTML = "example" + h$decodeUtf8z(html,0);
  return;
}
