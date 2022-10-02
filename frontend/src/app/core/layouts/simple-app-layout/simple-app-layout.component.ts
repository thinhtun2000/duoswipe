import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-simple-app-layout',
  templateUrl: './simple-app-layout.component.html',
  styleUrls: ['./simple-app-layout.component.scss'],
})
export class SimpleAppLayoutComponent implements OnInit {
  public currentDate: Date = new Date();

  constructor() {}

  ngOnInit(): void {}
}
