import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { LoginRequest } from '../../models/loginRequest';
import { UserApiService } from '../user-api/user-api.service';
import { UserService } from '../user/user.service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  constructor(
    private userApiSvc: UserApiService,
    private userSvc: UserService
  ) {}

  // public login(loginRequestObject: LoginRequest): Observable<any> {

  // }
}
