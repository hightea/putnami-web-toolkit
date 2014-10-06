package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class FacebookButton extends AbstractSocialButton {

	public FacebookButton() {
		this.setIconType(AbstractSocialButton.CONSTANT.facebookIcon());
		this.setTitle(AbstractSocialButton.CONSTANT.facebookPopover());
		this.setSocialUtlTemplate(AbstractSocialButton.CONSTANT.facebookUrl());
		this.setPopupHeight(500);
		this.setPopupWith(700);
	}
}
