/*
function myFunction() {
    document.getElementById("demo").style.color = "red";
}
function myFunction2() {
    document.getElementById("demo2").style.color = "blue";
}
*/
$('body').removeClass('modal-open');
$('body').removeAttr('style');


boxes = Array.from(document.getElementsByClassName('modal-backdrop show'));

boxes.forEach(box => {
  box.remove();
});
