import { Component, Input, OnInit } from '@angular/core';
import { User } from '../../models/user';

@Component({
  selector: 'app-profile-card',
  templateUrl: './profile-card.component.html',
  styleUrls: ['./profile-card.component.scss'],
})
export class ProfileCardComponent implements OnInit {
  @Input() users: User[];
  public index: number = 1;

  constructor() {}

  ngOnInit(): void {
    console.log(this.index);
  }

  public onClick(): void {
    this.index += 1;
    console.log(this.index);
  }
}
