import { Component, OnInit } from '@angular/core';
import { User } from 'src/app/core/models/test-user';

@Component({
  selector: 'app-user-profile',
  templateUrl: './user-profile.component.html',
  styleUrls: ['./user-profile.component.scss'],
})
export class UserProfileComponent implements OnInit {
  user1: User = {
    id: '12345',
    firstName: 'Test',
    lastName: 'One',
    userName: 'thisistest1',
  };

  constructor() {}

  ngOnInit(): void {}
}
