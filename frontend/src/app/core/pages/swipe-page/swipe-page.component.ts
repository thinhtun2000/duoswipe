import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { User } from '../../models/user';
import { SwipeService } from '../../services/swipe/swipe.service';

@Component({
  selector: 'app-swipe-page',
  templateUrl: './swipe-page.component.html',
  styleUrls: ['./swipe-page.component.scss'],
})
export class SwipePageComponent implements OnInit {
  public users: User[];
  public index: number = 0;
  @Output() changeUser: EventEmitter<number> = new EventEmitter<number>();

  constructor(private swipeSvc: SwipeService) {}

  ngOnInit(): void {
    this.swipeSvc.getUsers().subscribe((response) => {
      this.users = response;
      console.log(response);
    });
  }
}
