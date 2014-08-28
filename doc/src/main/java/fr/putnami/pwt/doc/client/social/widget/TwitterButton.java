package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class TwitterButton extends AbstractSocialButton {

	public TwitterButton() {
		setIconType(CONSTANT.twitterIcon());
		setTitle(CONSTANT.twitterPopover());
		setSocialUtlTemplate(CONSTANT.twitterUrl());
		setPopupHeight(250);
		setPopupWith(500);
	}

}
