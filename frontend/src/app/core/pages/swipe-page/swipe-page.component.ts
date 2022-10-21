import { Component, OnInit } from '@angular/core';
import { User } from '../../models/user';

@Component({
  selector: 'app-swipe-page',
  templateUrl: './swipe-page.component.html',
  styleUrls: ['./swipe-page.component.scss'],
})
export class SwipePageComponent implements OnInit {
  public users: Array<User> = [
    {
      user_id: '',
      name: '',
      password: '',
      email: '',
      language_id: 1,
      location_id: 1,
      pref_pos: 1,
      pref_lang: 1,
      pref_day: '',
      pref_time: '',
      pos_1: 1,
      pos_2: 1,
    },
  ];

  constructor() {}

  ngOnInit(): void {}
}
