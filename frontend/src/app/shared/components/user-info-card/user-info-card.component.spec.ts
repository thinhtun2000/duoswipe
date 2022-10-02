import { ComponentFixture, TestBed } from '@angular/core/testing';

import { UserInfoCardComponent } from './user-info-card.component';

describe('UserInfoCardComponent', () => {
  let component: UserInfoCardComponent;
  let fixture: ComponentFixture<UserInfoCardComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ UserInfoCardComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(UserInfoCardComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
