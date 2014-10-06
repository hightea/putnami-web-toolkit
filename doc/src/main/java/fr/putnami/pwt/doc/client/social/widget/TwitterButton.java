package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class TwitterButton extends AbstractSocialButton {

	public TwitterButton() {
		this.setIconType(AbstractSocialButton.CONSTANT.twitterIcon());
		this.setTitle(AbstractSocialButton.CONSTANT.twitterPopover());
		this.setSocialUtlTemplate(AbstractSocialButton.CONSTANT.twitterUrl());
		this.setPopupHeight(250);
		this.setPopupWith(500);
	}

}
