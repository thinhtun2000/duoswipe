import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { User } from 'src/app/core/models/user';
import { UserApiService } from 'src/app/core/services/user-api/user-api.service';
import { UserService } from 'src/app/core/services/user/user.service';

@Injectable({
  providedIn: 'root',
})
export class UserProfileService {
  constructor(private userApi: UserApiService, private userSvc: UserService) {}
}
