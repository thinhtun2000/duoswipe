import { Component, OnInit } from '@angular/core';
import { User } from '../../models/user';
import { MatchApiService } from '../../services/matched/match-api.service';
import { UserApiService } from '../../services/user-api/user-api.service';
import { UserService } from '../../services/user/user.service';

@Component({
  selector: 'app-match-page',
  templateUrl: './match-page.component.html',
  styleUrls: ['./match-page.component.scss'],
})
export class MatchPageComponent implements OnInit {
  public matched: any;

  constructor(private userSvc: UserService) {
    this.matched = this.userSvc._matched;
  }

  ngOnInit(): void {}
}
