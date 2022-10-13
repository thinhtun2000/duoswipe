import { Injectable } from '@angular/core';
import { BehaviorSubject, Observable } from 'rxjs';
import { User } from 'src/app/core/models/user';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Injectable({
  providedIn: 'root',
})
export class UserProfileService {
  public user: User;

  constructor(private userApiSvc: UserApiService) {}

  public getUserProfile(id: string) {
    this.userApiSvc.getUserById(id).subscribe((response) => {
      console.log(this.user);
    });
  }
}
