import { Component, OnInit } from '@angular/core';
import { User } from '../../models/user';
import { UserApiService } from '../../services/user-api/user-api.service';

@Component({
  selector: 'app-dashboard',
  templateUrl: './dashboard.component.html',
  styleUrls: ['./dashboard.component.scss'],
})
export class DashboardComponent implements OnInit {
  public user: User;

  constructor(private userApiSvc: UserApiService) {}

  ngOnInit(): void {
    this.userApiSvc.getUserById('1').subscribe((response) => {
      this.user = response;
      console.log(this.user);
    });
  }
}
