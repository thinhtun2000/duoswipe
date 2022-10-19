import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Observable } from 'rxjs';
import { User } from '../../models/user';
import { AuthService } from '../../services/auth/auth.service';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-app-layout',
  templateUrl: './app-layout.component.html',
  styleUrls: ['./app-layout.component.scss'],
})
export class AppLayoutComponent implements OnInit {
  public currentDate: Date = new Date();
  public user$: Observable<User | null>;

  constructor(
    private authSvc: AuthService,
    private router: Router,
    private userSvc: UserService
  ) {}

  ngOnInit(): void {
    this.user$ = this.userSvc.user$;
    this.user$.subscribe((user) => {});
  }

  logout() {
    this.authSvc.logout().subscribe((success) => {
      if (!success) return;
      this.router.navigateByUrl('/login');
    });
  }
}
