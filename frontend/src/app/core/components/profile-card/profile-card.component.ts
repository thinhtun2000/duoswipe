import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { User } from '../../models/user';
import { SwipePageComponent } from '../../pages/swipe-page/swipe-page.component';
import { UserApiService } from '../../services/user-api/user-api.service';

@Component({
  selector: 'app-profile-card',
  templateUrl: './profile-card.component.html',
  styleUrls: ['./profile-card.component.scss'],
})
export class ProfileCardComponent implements OnInit {
  @Input() user_id: string;
  @Input() index: number;
  @Input() max: number;
  public user: User;
  public offsetX: number = 0;
  public offsetY: number = 0;
  public static startPoint: any;
  public static topCard: ProfileCardComponent;
  @Output() static newMatch = new EventEmitter<any>();

  constructor(private userApi: UserApiService) {}

  ngOnInit(): void {
    console.log(this.user_id);
    this.userApi.getUserById(this.user_id).subscribe((user) => {
      this.user = user;
    });
  }

  public handleMouseDown(event: MouseEvent) {
    ProfileCardComponent.startPoint = { x: event.x, y: event.y };
    // Mouse could be up before it ever moves
    ProfileCardComponent.topCard = this;
    document.addEventListener('mouseup', ProfileCardComponent.handleMouseUp);
    document.addEventListener(
      'mousemove',
      ProfileCardComponent.handleMouseMove
    );
  }

  public static handleMouseMove(event: MouseEvent) {
    const card = document.getElementById('first')!;
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    if (ProfileCardComponent.startPoint != null) {
      ProfileCardComponent.topCard.offsetX =
        event.x - ProfileCardComponent.startPoint.x;
      ProfileCardComponent.topCard.offsetY =
        event.y - ProfileCardComponent.startPoint.y;
      const rotate = ProfileCardComponent.topCard.offsetX * 0.1;
      card.style.transform = `translate(${ProfileCardComponent.topCard.offsetX}px, ${ProfileCardComponent.topCard.offsetY}px) rotate(${rotate}deg)`;
      // if swipe right then show Like tag, if left show Pass tag
      const opacity = Math.abs(
        ProfileCardComponent.topCard.offsetX / (card.clientWidth * 0.4)
      );
      if (ProfileCardComponent.topCard.offsetX > 0)
        like.style.opacity = `${opacity}`;
      else if (ProfileCardComponent.topCard.offsetX == 0) {
        like.style.opacity = '0';
        nope.style.opacity = '0';
      } else nope.style.opacity = `${opacity}`;
    }
  }

  public static handleMouseUp(event: MouseEvent) {
    const card = document.getElementById('first')!;
    const like = document.getElementById('like')!;
    const nope = document.getElementById('nope')!;
    if (
      event.x > ProfileCardComponent.startPoint.x &&
      event.x - ProfileCardComponent.startPoint.x > 250
    ) {
      ProfileCardComponent.newMatch.emit(ProfileCardComponent.topCard.user_id);
      console.log(ProfileCardComponent.newMatch);
    } else if (
      event.x < ProfileCardComponent.startPoint.x &&
      ProfileCardComponent.startPoint.x - event.x > 250
    ) {
      console.log('swipe left');
    }
    ProfileCardComponent.startPoint = null;
    document.removeEventListener('mousemove', this.handleMouseMove, true);
    document.getElementById('first')!.style.transform = '';
    like.style.opacity = `0`;
    nope.style.opacity = `0`;
  }
}
