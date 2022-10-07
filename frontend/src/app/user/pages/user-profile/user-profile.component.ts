import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { User } from 'src/app/core/models/user';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Component({
  selector: 'app-user-profile',
  templateUrl: './user-profile.component.html',
  styleUrls: ['./user-profile.component.scss'],
})
export class UserProfileComponent implements OnInit {
  public user: User;
  @Output() eUser: EventEmitter<User> = new EventEmitter<User>();

  constructor(
    private userApiSvc: UserApiService,
    private userSvc: UserService
  ) {}

  ngOnInit(): void {
    this.userApiSvc.getUserById('1').subscribe((response) => {
      this.user = response;
      console.log(this.user);
    });
  }
}
