import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-match-page',
  templateUrl: './match-page.component.html',
  styleUrls: ['./match-page.component.scss']
})
export class MatchPageComponent implements OnInit {

  ngOnInit(): void {
  }
  
  myScriptElement: HTMLScriptElement;
 
  constructor(){
     this.myScriptElement = document.createElement("script");
     this.myScriptElement.src = "/assets/js/myjs.js";
     document.body.appendChild(this.myScriptElement);
  }

}
