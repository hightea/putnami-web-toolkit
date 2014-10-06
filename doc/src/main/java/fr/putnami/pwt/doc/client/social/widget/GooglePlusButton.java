package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class GooglePlusButton extends AbstractSocialButton {

	public GooglePlusButton() {
		this.setIconType(AbstractSocialButton.CONSTANT.gplusIcon());
		this.setTitle(AbstractSocialButton.CONSTANT.gplusPopover());
		this.setSocialUtlTemplate(AbstractSocialButton.CONSTANT.gplusUrl());
		// setPopupHeight(500);
		this.setPopupWith(500);
	}
}
