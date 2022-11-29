import { Component, OnInit } from '@angular/core';
import { User } from 'src/app/core/models/user';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Component({
  selector: 'app-user-profile-edit',
  templateUrl: './user-profile-edit.component.html',
  styleUrls: ['./user-profile-edit.component.scss'],
})
export class UserProfileEditComponent implements OnInit {
  public user: User | null;

  constructor(private userSvc: UserService, private userApi: UserApiService) {}

  ngOnInit(): void {
    this.userSvc.user$.subscribe((user) => {
      this.user = user;
      console.log(this.user);
    });
  }

  public onSubmit(toChange: any) {
    console.log(toChange);
    if (toChange.type == 'info') {
      const user = toChange.data;
      this.userApi.updateInfo(user.user_id, user).subscribe((response) => {
        console.log(response);
      });
    } else if (toChange.type == 'pref') {
      const user = toChange.data;
      this.userApi.updatePref(user.user_id, user).subscribe((response) => {
        console.log(response);
      });
    }
  }
}
