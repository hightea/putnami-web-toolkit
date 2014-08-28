package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class GooglePlusButton extends AbstractSocialButton {

	public GooglePlusButton() {
		setIconType(CONSTANT.gplusIcon());
		setTitle(CONSTANT.gplusPopover());
		setSocialUtlTemplate(CONSTANT.gplusUrl());
		//		setPopupHeight(500);
		setPopupWith(500);
	}
}
